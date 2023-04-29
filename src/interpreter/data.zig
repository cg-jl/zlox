const std = @import("std");
const ast = @import("../ast.zig");
const Core = @import("Core.zig");
const Walker = @import("Walker.zig");
const Token = @import("../Token.zig");
const Frame = @import("Frame.zig");
const Ctx = @import("Ctx.zig");
pub const Result = AllocOrSignal!Value;
pub const VoidResult = AllocOrSignal!void;

pub const Local = packed struct {
    line: u16,
    col: u32,

    pub const U = std.meta.Int(.unsigned, @bitSizeOf(@This()));
};

pub const Depth = struct {
    env: usize,
    stack: u32,
};

pub const FatStr = struct {
    alloc_refcount: ?usize,
    string: []const u8,
};

pub const Value = union(enum(u3)) {
    string: FatStr,
    num: f64,
    boolean: bool,
    callable: CallableVT,
    func: Function,
    class: *Class,
    instance: *Instance,
    nil: void,

    pub fn addRef(self: *Value) void {
        switch (self.*) {
            .string => |*s| if (s.alloc_refcount) |*r| {
                r.* = r.* + 1;
            },
            .func => |*f| f.refcount += 1,
            .class => |c| c.refcount += 1,
            .instance => |i| i.refcount += 1,
            else => {},
        }
    }

    pub fn depcount(self: *const Value) usize {
        if (self.* == .class) return self.class.instance_count +
            self.class.refcount;
        if (self.* == .instance) return self.instance.refcount;
        return 0;
    }

    // The value went out of scope
    pub fn dispose(self: *Value, state: *Core) void {
        if (self.* == .class) {
            // we have our own env for the super thing
            if (self.class.superclass) |super| {
                const env: ?*const Frame = getEnv: {
                    var it = self.class.methods.valueIterator();
                    const v = it.next() orelse break :getEnv null;
                    break :getEnv v.closure;
                };
                if (env) |e| {
                    state.restoreFrame(e.*);
                }
                super.refcount -= 1;
            }
            self.class.methods.deinit(state.arena.allocator());
            state.class_pool.destroy(self.class);
        } else if (self.* == .instance) {
            self.instance.refcount -= 1;
            if (self.instance.refcount == 0) {
                self.instance.class.instance_count -= 1;
                self.instance.fields.deinit(state.arena.allocator());
            }
        } else if (self.* == .string) {
            if (self.string.alloc_refcount) |r| {
                if (r == 0)
                    state.ctx.ally().free(self.string.string);
            }
        }
    }

    pub fn nil() Value {
        return .{ .nil = {} };
    }

    // count == 8 => u3

    pub fn isNum(v: Value) bool {
        return switch (v) {
            .num => |_| true,
            else => false,
        };
    }

    pub fn format(
        v: Value,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        switch (v) {
            .string => |s| try writer.print("{s}", .{s.string}),
            // TODO: make endsWith(".0") cut
            .num => |n| try writer.print("{}", .{n}),
            .boolean => |b| try writer.print("{}", .{b}),
            .callable => |f| try writer.print("{s}", .{f.repr}),
            .func => |f| try writer.print("{s}", .{if (f.decl.name) |n| n.lexeme else "lambda"}),
            .class => |c| try writer.print("{s}", .{c.name}),
            .instance => |i| try writer.print("{s} instance", .{i.class.name}),
            .nil => try writer.print("nil", .{}),
        }
    }
};

pub const Class = struct {
    name: []const u8,
    // *immutable*
    methods: std.StringHashMapUnmanaged(Function),
    // we can cache the init method when it's called since we cannot edit the "methods" list.
    init_method: ?Function,
    superclass: ?*Class,
    refcount: usize = 0,
    instance_count: usize = 0,
    fn call(p: *const anyopaque, i: *Walker, args: []const ast.Expr) Result {
        const class = @ptrCast(*const Class, @alignCast(@alignOf(Class), p));
        const instance: *Instance = try i.core.instance_pool.create();
        errdefer i.core.instance_pool.destroy(instance);
        if (class.init_method) |im| {
            const bound = try i.core.bind(im, instance);
            defer i.core.unbind(bound);
            try bound.makeInitCall(i, args);
        }
        return Value{ .instance = instance };
    }

    pub fn findMethod(class: *const Class, name: []const u8) ?Function {
        var cl = class;
        while (true) {
            if (cl.methods.get(name)) |m| return m;
            cl = cl.superclass orelse return null;
        }
    }

    pub fn getVT(class: *const Class) CallableVT {
        return CallableVT{
            .ptr = @ptrCast(*const anyopaque, @alignCast(1, class)),
            .arity = if (class.init_method) |m| @intCast(u8, m.decl.params.len) else 0,
            .call = &Class.call,
            .repr = undefined,
        };
    }
};
pub const Function = struct {
    decl: ast.FuncDecl,
    closure: *const Frame,
    refcount: usize = 0,

    inline fn setupCall(func: *const Function, st: *Walker, args: []const ast.Expr, frame: *Frame) AllocOrSignal!void {
        try st.core.values.ensureUnusedCapacity(st.core.arena.allocator(), args.len);
        for (args) |a| {
            st.core.values.appendAssumeCapacity(try st.visitExpr(a));
        }

        st.core.pushFrame(frame);

        // Make sure that the ancestor calls point to the correct environment.
        st.core.current_env.enclosing = func.closure;
        // Make sure that the arguments are popped too.
        // We don't create the frame before the arguments are evaluated
        // because then we introduce a new scope where it shouldn't be.
        st.core.current_env.values_begin -= args.len;
    }

    fn makeInitCall(func: *const Function, st: *Walker, args: []const ast.Expr) VoidResult {
        var frame: Frame = undefined;
        try func.setupCall(st, args, &frame);
        defer st.core.restoreFrame(frame);

        st.executeBlock(func.decl.body) catch |err| {
            if (err != error.Return) return err;
            if (st.core.ret_val) |*r| {
                r.dispose(&st.core);
            }
            st.core.ret_val = null;
        };
    }

    pub fn makeCall(ptr: *const anyopaque, st: *Walker, args: []const ast.Expr) Result {
        const func = @ptrCast(*const Function, @alignCast(@alignOf(Function), ptr));

        var frame: Frame = undefined;

        try func.setupCall(st, args, &frame);
        defer st.core.restoreFrame(frame);

        const ret_val: Value = catchReturn: {
            st.executeBlock(func.decl.body) catch |err| {
                if (err == error.Return) {
                    const ret = st.core.ret_val orelse Value.nil();
                    st.core.ret_val = null;
                    break :catchReturn ret;
                } else return err;
            };
            break :catchReturn Value.nil();
        };

        return ret_val;
    }

    pub fn getVT(f: *const Function) CallableVT {
        return CallableVT{
            .ptr = @ptrCast(*const anyopaque, f),
            .arity = @intCast(u8, f.decl.params.len),
            .call = &makeCall,
            // TODO: proper formatting using zig's writer API
            .repr = undefined, // please don't touch it!
        };
    }
};

// TODO: we already know what we can call. This only serves as a wrapper for native fns,
// and as a "common interface". Consider making this VT static over type?
pub const CallableVT = struct {
    ptr: *const anyopaque,
    arity: u8, // cannot be > 255
    repr: []const u8,
    call: *const fn (*const anyopaque, *Walker, []const ast.Expr) Result,
};

pub const Signal = error{ RuntimeError, Return };
pub const AllocErr = std.mem.Allocator.Error;
pub const AllocOrSignal = Signal || std.mem.Allocator.Error;
pub const Error = struct {
    token: Token,
    message: []const u8,
};

pub const Instance = struct {
    class: *Class,
    fields: std.StringHashMapUnmanaged(Value) = .{},
    refcount: usize,
};

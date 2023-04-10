const std = @import("std");
const ast = @import("../ast.zig");
const State = @import("State.zig");
const Token = @import("../Token.zig");
const Env = @import("Env.zig");
const Ctx = @import("Ctx.zig");
pub const Result = AllocOrSignal!Value;

pub const Value = union(enum(u3)) {
    string: []const u8,
    num: f64,
    boolean: bool,
    callable: CallableVT,
    func: Function,
    class: Class,
    instance: *Instance,
    nil: void,

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
            .string => |s| try writer.print("{s}", .{s}),
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
    superclass: ?*const Class,
    fn call(p: *const anyopaque, i: *State, args: []const Value) Result {
        const class = @ptrCast(*const Class, @alignCast(@alignOf(Class), p));
        const instance: *Instance = try i.instance_pool.create();
        errdefer i.instance_pool.destroy(instance);
        if (class.init_method) |im| {
            const bound = try i.bind(im, instance);
            defer i.unbind(bound);
            _ = try bound.getVT().call(&bound, i, args);
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
    closure: *Env,
    is_init: bool,

    pub fn makeCall(ptr: *const anyopaque, st: *State, args: []const Value) Result {
        const func = @ptrCast(*const Function, @alignCast(@alignOf(Function), ptr));
        // Load in the new environment
        const env: *Env = try st.env_pool.create();
        defer st.env_pool.destroy(env);
        env.* = Env{ .enclosing = func.closure };
        for (func.decl.params, 0..) |param, i| {
            try env.define(st.arena.allocator(), param.lexeme, args[i]);
        }

        const ret_val: Value = catchReturn: {
            st.executeBlockIn(func.decl.body, env) catch |err| {
                if (err == error.Return) {
                    const ret = st.ret_val orelse Value.nil();
                    st.ret_val = null;
                    break :catchReturn ret;
                } else return err;
            };
            break :catchReturn Value.nil();
        };

        if (func.is_init) return func.closure.getAt(0, "this") orelse unreachable;
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
    call: *const fn (*const anyopaque, *State, []const Value) Result,
};

pub const Signal = error{ RuntimeError, Return };
pub const AllocErr = std.mem.Allocator.Error;
pub const AllocOrSignal = Signal || std.mem.Allocator.Error;
pub const Error = struct {
    token: Token,
    message: []const u8,
};

pub const Instance = struct {
    class: *const Class,
    fields: std.StringHashMapUnmanaged(Value) = .{},
};

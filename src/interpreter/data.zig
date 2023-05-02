const std = @import("std");
const Ast = @import("../ast/Ast.zig");
const Core = @import("Core.zig");
const Token = @import("../Token.zig");
const Frame = @import("Frame.zig");
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

pub inline fn literalToValue(tok: Token) Value {
    return switch (tok.ty) {
        .NUMBER => .{
            .num = std.fmt.parseFloat(f64, tok.source.lexeme) catch unreachable,
        },
        .STRING => .{ .string = .{
            .string = tok.source.lexeme[1 .. tok.source.lexeme.len - 1],
            .alloc_refcount = null,
        } },
        .TRUE => .{ .boolean = true },
        .FALSE => .{ .boolean = false },
        .NIL => .{ .nil = {} },
        else => unreachable,
    };
}

pub const Value = union(enum(u3)) {
    string: FatStr,
    num: f64,
    boolean: bool,
    func: Function,
    class: *Class,
    instance: *Instance,
    nil: void,
    builtin_clock: void,

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

    // The value went out of scope
    pub fn dispose(self: *Value, core: *Core) void {
        if (self.* == .class) {
            if (self.class.refcount == 0) {
                // we have our own env for the super thing
                if (self.class.superclass) |super| {
                    const env: ?*const Frame = getEnv: {
                        var it = self.class.methods.valueIterator();
                        const v = it.next() orelse break :getEnv null;
                        break :getEnv v.closure;
                    };
                    if (env) |e| {
                        core.restoreFrame(e.*);
                    }
                    super.refcount -= 1;
                }
                self.class.methods.deinit(core.arena.allocator());
                core.class_pool.destroy(self.class);
            }
        } else if (self.* == .instance) {
            self.instance.refcount -= 1;
            if (self.instance.refcount == 0) {
                self.instance.class.instance_count -= 1;
                self.instance.fields.deinit(core.arena.allocator());
            }
        } else if (self.* == .string) {
            if (self.string.alloc_refcount) |r| {
                if (r == 0)
                    core.ctx.ally().free(self.string.string);
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
            .builtin_clock => try writer.print("<native fn 'clock'>", .{}),
            .func => |_| try writer.print("fun", .{}),
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

    pub fn findMethod(class: *const Class, name: []const u8) ?Function {
        var cl = class;
        while (true) {
            if (cl.methods.get(name)) |m| return m;
            cl = cl.superclass orelse return null;
        }
    }
};
pub const Function = struct {
    decl: Ast.Node.FuncDecl,
    closure: *const Frame,
    refcount: usize = 0,
};

pub const Signal = error{ RuntimeError, Return };
pub const AllocErr = std.mem.Allocator.Error;
pub const AllocOrSignal = Signal || std.mem.Allocator.Error;
pub const Error = struct {
    source: Token.Source,
    message: []const u8,
};

pub const Instance = struct {
    class: *Class,
    fields: std.StringHashMapUnmanaged(Value) = .{},
    refcount: usize,
};

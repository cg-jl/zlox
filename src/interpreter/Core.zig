const std = @import("std");
const data = @import("data.zig");
const Frame = @import("Frame.zig");
const context = @import("../context.zig");
const Ctx = @import("Ctx.zig");
const Token = @import("../Token.zig");
const AllocErr = std.mem.Allocator.Error;
const Resolver = @import("NodeResolver.zig");

const Depth = data.Depth;
const local = Resolver.local;

const Core = @This();
const Result = data.Result;
const VoidResult = data.AllocOrSignal!void;

// TODO: add a GPA with underlying arena {.underlying_allocator = arena} to the
// state, for the env stuff

ctx: Ctx,
current_env: Frame,
values: std.ArrayListUnmanaged(data.Value),
// to be able to bind functions properly, we have to track instances
// apart
instance_pool: std.heap.MemoryPool(data.Instance),
env_pool: std.heap.MemoryPool(Frame),
class_pool: std.heap.MemoryPool(data.Class),
arena: std.heap.ArenaAllocator,
timer: std.time.Timer,
ret_val: ?data.Value = null,

pub fn init(gpa: std.mem.Allocator, value_capacity: usize) !Core {
    var arena_gpa = std.heap.ArenaAllocator.init(gpa);
    errdefer arena_gpa.deinit();

    var values = try std.ArrayListUnmanaged(data.Value).initCapacity(
        arena_gpa.allocator(),
        value_capacity,
    );

    const globals = Frame{ .enclosing = undefined, .values_begin = 0 };

    return Core{
        .ctx = Ctx.init(gpa),
        .current_env = globals,
        .env_pool = std.heap.MemoryPool(Frame).init(gpa),
        .values = values,
        .instance_pool = std.heap.MemoryPool(data.Instance).init(gpa),
        .class_pool = std.heap.MemoryPool(data.Class).init(gpa),
        .arena = arena_gpa,
        // let's pretend this cannot error for now. Misusing a toy language interpreter
        // in a seccomp environment is hard to do.
        .timer = std.time.Timer.start() catch unreachable,
    };
}

pub fn deinit(core: *Core) void {
    core.env_pool.deinit();
    core.class_pool.deinit();
    core.instance_pool.deinit();
    core.arena.deinit();
    core.ctx.deinit();
}

pub inline fn pushFrame(core: *Core, frame: *Frame) void {
    frame.* = core.current_env;
    core.current_env = .{
        .enclosing = undefined,
        .values_begin = core.values.items.len,
    };
}

fn disposeValues(core: *Core, vs: []data.Value) void {
    for (vs) |*v| {
        v.dispose(core);
    }
}

pub inline fn restoreFrame(core: *Core, frame: Frame) void {
    core.disposeValues(core.values.items[core.current_env.values_begin..]);
    core.values.items.len = core.current_env.values_begin;
    core.current_env = frame;
}

pub inline fn checkInstancePut(storage: *Core, this: data.Value, report_token: Token) data.Signal!*data.Instance {
    if (this == .instance) return this.instance;

    storage.ctx.report(report_token, "Only instances have values");

    return error.RuntimeError;
}

pub inline fn instancePut(
    storage: *Core,
    instance: *data.Instance,
    name: Token,
    value: data.Value,
) AllocErr!void {
    try instance.fields.put(storage.arena.allocator(), name.source.lexeme, value);
}

pub fn instanceGet(storage: *Core, this: data.Value, token: Token) Result {
    const instance: *data.Instance = if (this == .instance) this.instance else {
        storage.ctx.report(token, "Only instances have properties");
        return error.RuntimeError;
    };
    if (instance.fields.get(token.source.lexeme)) |f| return f;
    if (instance.class.findMethod(token.source.lexeme)) |m| {
        return .{ .func = try storage.bind(m, instance) };
    }
    storage.ctx.report(token, try std.fmt.allocPrint(
        storage.ctx.ally(),
        "Undefined property '{s}'",
        .{token.source.lexeme},
    ));
    return error.RuntimeError;
}

pub inline fn checkCallArgCount(
    core: *Core,
    expected: usize,
    got: usize,
    report_token: Token,
) data.AllocOrSignal!void {
    if (expected != got) {
        core.ctx.report(report_token, try std.fmt.allocPrint(
            core.ctx.ally(),
            "Expected {} arguments but got {}",
            .{ expected, got },
        ));
    }
}

pub inline fn valueAt(core: *Core, depth: data.Depth) *data.Value {
    const frame = core.current_env.ancestor(depth.env);
    return &core.values.items[frame.values_begin..][depth.stack];
}

pub inline fn endUnary(
    core: *Core,
    right: data.Value,
    op: Token,
) data.Result {
    switch (op.ty) {
        .BANG => return .{ .boolean = !isTruthy(right) },
        .MINUS => switch (right) {
            .num => |n| return .{ .num = -n },
            else => {
                core.ctx.report(op, "Operand must be a number");
                return error.RuntimeError;
            },
        },
        else => unreachable,
    }
}
pub inline fn isTruthy(obj: data.Value) bool {
    return switch (obj) {
        .nil => false,
        .boolean => |b| b,
        else => true,
    };
}

pub inline fn endBinary(
    core: *Core,
    left: data.Value,
    right: data.Value,
    op: Token,
) data.Result {
    switch (op.ty) {
        .MINUS => {
            const checked = try core.checkNumberOperands(op, left, right);
            return .{ .num = checked.left - checked.right };
        },
        .SLASH => {
            const checked = try core.checkNumberOperands(op, left, right);
            return .{ .num = checked.left / checked.right };
        },
        .STAR => {
            const checked = try core.checkNumberOperands(op, left, right);
            return .{ .num = checked.left * checked.right };
        },
        .PLUS => {
            switch (left) {
                .num => |ln| switch (right) {
                    .num => |rn| return .{ .num = ln + rn },
                    else => {},
                },
                .string => |ls| switch (right) {
                    .string => |rs| return .{
                        .string = .{
                            .string = try std.fmt.allocPrint(
                                core.ctx.ally(),
                                "{s}{s}",
                                .{ ls.string, rs.string },
                            ),
                            .alloc_refcount = 0,
                        },
                    },
                    else => {},
                },
                else => {},
            }

            core.ctx.report(op, "Operands must be two numbers or two strings");
            return error.RuntimeError;
        },
        .GREATER => {
            const checked = try core.checkNumberOperands(op, left, right);
            return .{ .boolean = checked.left > checked.right };
        },
        .GREATER_EQUAL => {
            const checked = try core.checkNumberOperands(op, left, right);
            return .{ .boolean = checked.left >= checked.right };
        },
        .LESS => {
            const checked = try core.checkNumberOperands(op, left, right);
            return .{ .boolean = checked.left < checked.right };
        },
        .LESS_EQUAL => {
            const checked = try core.checkNumberOperands(op, left, right);
            return .{ .boolean = checked.left <= checked.right };
        },

        // unknown op
        else => unreachable,
    }
}
fn checkNumberOperands(
    core: *Core,
    op: Token,
    left: data.Value,
    right: data.Value,
) data.Signal!struct { left: f64, right: f64 } {
    switch (left) {
        .num => |ln| {
            switch (right) {
                .num => |rn| return .{ .left = ln, .right = rn },
                else => {},
            }
        },
        else => {},
    }
    core.ctx.report(op, "Operands must be numbers");
    return error.RuntimeError;
}

pub inline fn superGet(core: *Core, this: data.Value, method_tok: Token) Result {
    const instance = if (this == .instance) this.instance else unreachable;
    const superclass: *const data.Class = instance.class.superclass.?;
    const method: data.Function = superclass.findMethod(method_tok.source.lexeme) orelse {
        core.ctx.report(method_tok, try std.fmt.allocPrint(
            core.ctx.ally(),
            "Undefined property '{s}'",
            .{method_tok.source.lexeme},
        ));
        return error.RuntimeError;
    };

    return .{ .func = try core.bind(method, instance) };
}

pub inline fn unbind(core: *Core, f: data.Function) void {
    core.restoreFrame(f.closure.*);
}

pub fn bind(
    core: *Core,
    f: data.Function,
    instancep: *data.Instance,
) AllocErr!data.Function {
    // This environment has to be on the heap, since we're making a reference
    // out of it.
    const env: *Frame = try core.env_pool.create();
    errdefer core.env_pool.destroy(env);
    env.* = core.current_env;

    // this sucks, because now I have to make instances pointers. Not cool!
    try core.values.append(core.arena.allocator(), .{ .instance = instancep });
    instancep.refcount += 1;

    return data.Function{
        .decl = f.decl,
        .closure = env,
    };
}

pub inline fn getClock(core: *Core) data.Value {
    const conversion = comptime @intToFloat(f64, std.time.ns_per_s);
    const value = @intToFloat(f64, core.timer.read());
    return .{ .num = value / conversion };
}

pub inline fn takeReturn(core: *Core) data.Value {
    if (core.ret_val) |v| {
        core.ret_val = null;
        return v;
    }
    return data.Value.nil();
}

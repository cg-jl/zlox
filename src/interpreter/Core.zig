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

pub inline fn checkInstancePut(
    storage: *Core,
    this: data.Value,
    report_source: Token.Source,
) data.Signal!*data.Instance {
    if (this == .instance) return this.instance;

    storage.ctx.report(report_source, "Only instances have values");

    return error.RuntimeError;
}

pub inline fn instancePut(
    storage: *Core,
    instance: *data.Instance,
    name: []const u8,
    value: data.Value,
) AllocErr!void {
    try instance.fields.put(storage.arena.allocator(), name, value);
}

pub fn instanceGet(storage: *Core, this: data.Value, src: Token.Source) Result {
    const instance: *data.Instance = if (this == .instance) this.instance else {
        storage.ctx.report(src, "Only instances have properties");
        return error.RuntimeError;
    };
    if (instance.fields.get(src.lexeme)) |f| return f;
    if (instance.class.findMethod(src.lexeme)) |m| {
        return .{ .func = try storage.bind(m, instance) };
    }
    storage.ctx.report(src, "Undefined property");
    return error.RuntimeError;
}

pub inline fn valueAt(core: *Core, depth: data.Depth) *data.Value {
    const frame = core.current_env.ancestor(depth.env);
    return &core.values.items[frame.values_begin..][depth.stack];
}
pub inline fn isTruthy(obj: data.Value) bool {
    return switch (obj) {
        .nil => false,
        .boolean => |b| b,
        else => true,
    };
}

pub fn valuesEqual(left: data.Value, right: data.Value) bool {
    if (std.meta.activeTag(left) != std.meta.activeTag(right)) return false;
    switch (std.meta.activeTag(left)) {
        .num => return right.num == left.num,
        .string => return std.mem.eql(u8, left.string.string, right.string.string),
        .func => return right.func.decl.body.start == left.func.decl.body.start,
        .class => return right.class == left.class,
        .instance => return right.instance == left.instance,
        .boolean => return right.boolean == left.boolean,
        else => return true,
    }
}

pub inline fn superGet(
    core: *Core,
    this: data.Value,
    method_src: Token.Source,
) Result {
    const instance = if (this == .instance) this.instance else unreachable;
    const superclass: *const data.Class = instance.class.superclass.?;
    const method: data.Function = superclass.findMethod(method_src.lexeme) orelse {
        core.ctx.report(method_src, "Undefined property");
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
    const conversion = comptime @as(f64, @floatFromInt(std.time.ns_per_s));
    const value = @as(f64, @floatFromInt(core.timer.read()));
    return .{ .num = value / conversion };
}

pub inline fn takeReturn(core: *Core) data.Value {
    if (core.ret_val) |v| {
        core.ret_val = null;
        return v;
    }
    return data.Value.nil();
}

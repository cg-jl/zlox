const std = @import("std");
const data = @import("data.zig");
const Frame = @import("Frame.zig");
const ast = @import("../ast.zig");
const context = @import("../context.zig");
const Ctx = @import("Ctx.zig");
const Token = @import("../Token.zig");
const AllocErr = std.mem.Allocator.Error;
const Resolver = @import("Resolver.zig");

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

pub fn pushFrame(core: *Core, frame: *Frame) void {
    frame.* = core.current_env;
    core.current_env = .{
        .enclosing = undefined,
        .values_begin = core.values.items.len,
    };
}

fn disposeValues(core: *Core, vs: []data.Value) void {
    for (vs) |*v| {
        if (v.depcount() == 0) v.dispose(core);
    }
}

pub fn restoreFrame(core: *Core, frame: Frame) void {
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
    try instance.fields.put(storage.arena.allocator(), name.lexeme, value);
}

pub fn instanceGet(storage: *Core, this: data.Value, token: Token) Result {
    const instance: *data.Instance = if (this == .instance) this.instance else {
        storage.ctx.report(token, "Only instances have properties");
        return error.RuntimeError;
    };
    if (instance.fields.get(token.lexeme)) |f| return f;
    if (instance.class.findMethod(token.lexeme)) |m| {
        return .{ .func = try storage.bind(m, instance) };
    }
    storage.ctx.report(token, try std.fmt.allocPrint(
        storage.ctx.ally(),
        "Undefined property '{s}'",
        .{token.lexeme},
    ));
    return error.RuntimeError;
}

pub fn superGet(core: *Core, this: data.Value, method_tok: Token) Result {
    const instance = if (this == .instance) this.instance else unreachable;
    const superclass: *const data.Class = instance.class.superclass.?;
    const method: data.Function = superclass.findMethod(method_tok.lexeme) orelse {
        core.ctx.report(method_tok, try std.fmt.allocPrint(
            core.ctx.ally(),
            "Undefined property '{s}'",
            .{method_tok.lexeme},
        ));
        return error.RuntimeError;
    };

    return .{ .func = try core.bind(method, instance) };
}

pub fn unbind(core: *Core, f: data.Function) void {
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
        .is_init = f.is_init,
    };
}

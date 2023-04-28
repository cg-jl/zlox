const std = @import("std");
const data = @import("data.zig");
const Env = @import("Env.zig");
const ast = @import("../ast.zig");
const context = @import("../context.zig");
const Ctx = @import("Ctx.zig");
const Token = @import("../Token.zig");
const AllocErr = std.mem.Allocator.Error;
const Resolver = @import("Resolver.zig");

const Depth = data.Depth;
const local = Resolver.local;

const State = @This();
const Result = data.Result;
const VoidResult = data.AllocOrSignal!void;

pub const EnvHandle = usize;

// TODO: add a GPA with underlying arena {.underlying_allocator = arena} to the
// state, for the env stuff

ctx: Ctx,
// This is taking a lot of hits, even if it's just storing positions and their
// respective code locations.
locals: Resolver.LocalMap,
current_env: Env,
values: std.ArrayListUnmanaged(data.Value),
// to be able to bind functions properly, we have to track instances
// apart
instance_pool: std.heap.MemoryPool(data.Instance),
env_pool: std.heap.MemoryPool(Env),
class_pool: std.heap.MemoryPool(data.Class),
arena: std.heap.ArenaAllocator,
timer: std.time.Timer,
ret_val: ?data.Value = null,

pub fn init(gpa: std.mem.Allocator) !State {
    var arena_gpa = std.heap.ArenaAllocator.init(gpa);
    errdefer arena_gpa.deinit();

    var values = try std.ArrayListUnmanaged(data.Value).initCapacity(
        arena_gpa.allocator(),
        1,
    );

    const globals = Env{ .enclosing = undefined, .values_begin = 0 };
    values.appendAssumeCapacity(
        .{ .callable = data.CallableVT{
            .ptr = undefined,
            .repr = "<native fn clock()>",
            .call = &getClock,
            .arity = 0,
        } },
    );

    return State{
        .ctx = Ctx.init(gpa),
        .current_env = globals,
        .locals = .{},
        .env_pool = std.heap.MemoryPool(Env).init(gpa),
        .values = values,
        .instance_pool = std.heap.MemoryPool(data.Instance).init(gpa),
        .class_pool = std.heap.MemoryPool(data.Class).init(gpa),
        .arena = arena_gpa,
        // let's pretend this cannot error for now. Misusing a toy language interpreter
        // in a seccomp environment is hard to do.
        .timer = std.time.Timer.start() catch unreachable,
    };
}

pub fn deinit(state: *State) void {
    state.env_pool.deinit();
    state.class_pool.deinit();
    state.instance_pool.deinit();
    state.arena.deinit();
    state.ctx.deinit();
}

pub fn pushFrame(state: *State, frame: *Env) void {
    frame.* = state.current_env;
    state.current_env = .{
        .enclosing = frame,
        .values_begin = state.values.items.len,
    };
}

fn disposeValues(state: *State, vs: []data.Value) void {
    for (vs) |*v| {
        if (v.depcount() == 0) v.dispose(state);
    }
}

pub fn restoreFrame(state: *State, frame: Env) void {
    state.disposeValues(state.values.items[state.current_env.values_begin..]);
    state.values.items.len = state.current_env.values_begin;
    state.current_env = frame;
}

pub fn tryPrintExpr(state: *State, e: ast.Expr) AllocErr!void {
    const v = state.visitExpr(e) catch |err| {
        switch (err) {
            error.Return => {
                std.log.err("Caught return in global context", .{});
                context.has_errored = true;
                return;
            },
            error.RuntimeError => {
                const a: data.Error = state.ctx.last_error orelse unreachable;
                std.log.err("@ line {} '{s}': {s}", .{ a.token.line, a.token.lexeme, a.message });
                context.has_errored = true;
                return;
            },
            else => return @errSetCast(AllocErr, err),
        }
    };

    std.debug.print("{}\n", .{v});
}

pub fn tryExecBlock(state: *State, stmts: []const ast.Stmt) AllocErr!void {
    var frame: Env = undefined;
    state.pushFrame(&frame);
    defer state.restoreFrame(frame);
    state.executeBlock(stmts) catch |err| {
        switch (err) {
            error.Return => {
                std.log.err("Caught return in global context", .{});
                return;
            },
            error.RuntimeError => {
                const a: data.Error = state.ctx.last_error orelse unreachable;
                std.log.err("@ line {} '{s}': {s}", .{ a.token.line, a.token.lexeme, a.message });
                return;
            },
            else => return @errSetCast(AllocErr, err),
        }
    };
}

pub fn stringify(state: *State, obj: data.Value) ![]const u8 {
    return try std.fmt.allocPrint(state.ctx.ally(), "{}", .{obj.*});
}

fn getClock(_: *const anyopaque, state: *State, _: []const ast.Expr) Result {
    const conversion = comptime @intToFloat(f64, std.time.ns_per_s);
    const value = @intToFloat(f64, state.timer.read());
    return .{ .num = value / conversion };
}

fn visitClass(state: *State, stmt: ast.Stmt.Class) VoidResult {
    const class_value_ptr = try state.values.addOne(state.arena.allocator());

    var superclass: ?*data.Class = null;
    const class_env = if (stmt.superclass) |sp| hasSuper: {
        const super = try state.lookupVariable(sp);
        const superc: *data.Class = switch (super) {
            .class => |c| c,
            else => {
                state.ctx.report(sp, "Super class must be a class");
                return error.RuntimeError;
            },
        };

        // we can only have one level of inheritance.
        // So we can have the class have a refcount

        const class_env = try state.env_pool.create();
        state.pushFrame(class_env);
        superc.refcount += 1;
        // It's ok to make a copy of the class info since they're immutable.
        try state.values.append(state.arena.allocator(), .{ .class = superc });
        superclass = superc;
        break :hasSuper class_env;
    } else cloneCurrent: {
        const clone = try state.env_pool.create();
        clone.* = state.current_env;
        break :cloneCurrent clone;
    };

    errdefer {
        if (stmt.superclass) |_| state.restoreFrame(class_env.*);
        state.env_pool.destroy(class_env);
    }

    var methods: std.StringHashMapUnmanaged(data.Function) = .{};

    var init_method: ?data.Function = null;
    for (stmt.methods) |m| {
        const method = data.Function{
            .decl = m.decl,
            .closure = class_env,
            .is_init = std.mem.eql(u8, m.name.lexeme, "init"),
        };
        if (method.is_init) init_method = method;
        try methods.put(state.arena.allocator(), m.name.lexeme, method);
    }

    const class = data.Class{
        .superclass = superclass,
        .methods = methods,
        .init_method = init_method,
        .name = stmt.name.lexeme,
    };

    const classp = try state.class_pool.create();
    classp.* = class;

    class_value_ptr.* = .{ .class = classp };
}

fn visitBlock(state: *State, block: []const ast.Stmt) VoidResult {
    var frame: Env = undefined;
    state.pushFrame(&frame);
    defer state.restoreFrame(frame);

    try state.executeBlock(block);
}

/// Execute a block without pushing an environment.
pub fn executeBlock(
    state: *State,
    block: []const ast.Stmt,
) VoidResult {
    for (block) |s| {
        try state.visitStmt(s);
    }
}

fn visitWhile(state: *State, wh: ast.Stmt.While) VoidResult {
    while (isTruthy(try state.visitExpr(wh.condition))) {
        try state.visitStmt(wh.body.*);
    }
}

fn visitVarStmt(state: *State, v: ast.Stmt.Var) VoidResult {
    var value: data.Value = .{ .nil = {} };
    if (v.init) |i| {
        value = try state.visitExpr(i);
    }

    try state.values.append(state.arena.allocator(), value);
}

fn visitPrint(state: *State, p: ast.Stmt.Print) VoidResult {
    const value = try state.visitExpr(p);
    std.debug.print("{s}\n", .{value});
}

inline fn visitReturn(state: *State, ret: ast.Stmt.Return) VoidResult {
    const value = try state.visitExpr(ret.value);
    state.ret_val = value;
    return error.Return;
}

fn visitIf(state: *State, iff: ast.Stmt.If) VoidResult {
    const cond = try state.visitExpr(iff.condition);
    if (isTruthy(cond)) return try state.visitStmt(iff.then_branch.*);
    if (iff.else_branch) |e| return try state.visitStmt(e.*);
}

fn visitFunction(state: *State, e: ast.Stmt.Function) VoidResult {
    const clone: *Env = try state.env_pool.create();
    clone.* = state.current_env;
    const function = data.Function{
        .decl = e.decl,
        .closure = clone,
        .is_init = false,
    };
    try state.values.append(state.arena.allocator(), .{ .func = function });
}

fn visitExprStmt(state: *State, e: ast.Expr) VoidResult {
    var v = try state.visitExpr(e);
    v.dispose(state);
}

fn visitSet(state: *State, e: ast.Expr.Set) Result {
    const obj: data.Value = try state.visitExpr(e.obj.*);
    const instance: *data.Instance = switch (obj) {
        .instance => |i| i,
        else => {
            state.ctx.report(e.name, "Only instances have fields");
            return error.RuntimeError;
        },
    };

    const val = try state.visitExpr(e.value.*);
    try state.instancePut(instance, e.name, val);
    return val;
}

fn visitGet(state: *State, e: ast.Expr.Get) Result {
    const obj: data.Value = try state.visitExpr(e.obj.*);
    switch (obj) {
        .instance => |i| return try state.instanceGet(i, e.name),
        else => {
            state.ctx.report(e.name, "Only instances have properties");
            return error.RuntimeError;
        },
    }
}

fn instancePut(
    state: *State,
    instance: *data.Instance,
    name: Token,
    value: data.Value,
) AllocErr!void {
    try instance.fields.put(state.arena.allocator(), name.lexeme, value);
}

fn instanceGet(state: *State, instance: *data.Instance, token: Token) Result {
    if (instance.fields.get(token.lexeme)) |f| return f;
    if (instance.class.findMethod(token.lexeme)) |m| {
        return .{ .func = try state.bind(m, instance) };
    }
    state.ctx.report(token, try std.fmt.allocPrint(
        state.ctx.ally(),
        "Undefined property '{s}'",
        .{token.lexeme},
    ));
    return error.RuntimeError;
}

fn visitSuper(state: *State, e: ast.Expr.Super) Result {
    const distance = state.locals.get(Resolver.local(e.keyword)) orelse unreachable;

    const this: data.Value = getThis: {
        const at = state.current_env.ancestor(distance.env);
        break :getThis state.values.items[at.values_begin..][distance.stack];
    };

    const obj: *data.Instance = switch (this) {
        .instance => |i| i,
        else => unreachable,
    };

    const superclass: *const data.Class = obj.class.superclass.?;

    const method: data.Function = superclass.findMethod(e.method.lexeme) orelse {
        state.ctx.report(e.method, try std.fmt.allocPrint(
            state.ctx.ally(),
            "Undefined property '{s}'",
            .{e.method.lexeme},
        ));
        return error.RuntimeError;
    };

    return .{ .func = try state.bind(method, obj) };
}

pub fn unbind(state: *State, f: data.Function) void {
    state.restoreFrame(f.closure.*);
}

pub fn bind(
    state: *State,
    f: data.Function,
    instancep: *data.Instance,
) AllocErr!data.Function {
    // This environment has to be on the heap, since we're making a reference
    // out of it.
    const env: *Env = try state.env_pool.create();
    errdefer state.env_pool.destroy(env);
    state.pushFrame(env);
    errdefer state.restoreFrame(env.*);

    // this sucks, because now I have to make instances pointers. Not cool!
    try state.values.append(state.arena.allocator(), .{ .instance = instancep });
    instancep.refcount += 1;

    return data.Function{
        .decl = f.decl,
        .closure = env,
        .is_init = f.is_init,
    };
}

fn visitThis(state: *State, this: Token) Result {
    return state.lookupVariable(this);
}

fn visitCall(state: *State, call: ast.Expr.Call) Result {
    const callee = try state.visitExpr(call.callee.*);
    const vt: data.CallableVT = switch (callee) {
        .func => |*f| f.getVT(),
        .class => |c| data.Class.getVT(c),
        .callable => |t| t,
        else => {
            state.ctx.report(call.paren, "Can only call functions and classes");
            return error.RuntimeError;
        },
    };

    if (call.arguments.len != vt.arity) {
        state.ctx.report(call.paren, try std.fmt.allocPrint(
            state.ctx.ally(),
            "Expected {} arguments but got {}",
            .{ vt.arity, call.arguments.len },
        ));
        return error.RuntimeError;
    }

    return try vt.call(vt.ptr, state, call.arguments);
}

fn visitAssign(state: *State, assign: ast.Expr.Assign) Result {
    var value = try state.visitExpr(assign.value.*);

    const distance: Depth = state.locals.get(local(assign.name)) orelse @panic("Unresolved variable");
    const ancestor = state.current_env.ancestor(distance.env);
    state.values.items[ancestor.values_begin..][distance.stack] = value;

    return value;
}

fn visitLambda(state: *State, lam: ast.Expr.Lambda) Result {
    const env = try state.env_pool.create();
    env.* = state.current_env;
    return .{ .func = data.Function{
        .decl = lam.decl,
        .closure = env,
        .is_init = false,
    } };
}

fn visitVar(state: *State, v: ast.Expr.Var) Result {
    return try state.lookupVariable(v);
}

fn lookupVariable(state: *State, v: Token) Result {
    const distance: Depth = state.locals.get(local(v)) orelse @panic("unresolved variable");
    const env = state.current_env.ancestor(distance.env);
    // NOTE: from perf reports, this is where the most amount of cache misses
    // occur during execution. Looks like accessing the variable 'n' still makes
    // us miss a ton on performance.
    return state.values.items[env.values_begin..][distance.stack];
}

fn visitUnary(state: *State, u: ast.Expr.Unary) Result {
    const right = try state.visitExpr(u.right.*);

    switch (u.operator.ty) {
        .BANG => return .{ .boolean = !isTruthy(right) },
        .MINUS => {
            switch (right) {
                .num => |n| return .{ .num = -n },
                else => {
                    state.ctx.report(u.operator, "Operand must be a number");
                    return error.RuntimeError;
                },
            }
        },
        else => return .{ .nil = {} },
    }
}

fn isTruthy(obj: data.Value) bool {
    return switch (obj) {
        .nil => false,
        .boolean => |b| b,
        else => true,
    };
}

fn visitLiteral(_: *State, l: ast.Expr.Literal) Result {
    return switch (l) {
        .string => |s| .{ .string = .{ .string = s, .was_allocated = false } },
        .num => |n| .{ .num = n },
        .boolean => |b| .{ .boolean = b },
        .nil => .{ .nil = {} },
    };
}

fn visitBinary(state: *State, b: ast.Expr.Binary) Result {
    const left = try state.visitExpr(b.left.*);
    const right = try state.visitExpr(b.right.*);

    switch (b.operator.ty) {
        .MINUS => {
            const checked = try state.checkNumberOperands(b.operator, left, right);
            return .{ .num = checked.left - checked.right };
        },
        .SLASH => {
            const checked = try state.checkNumberOperands(b.operator, left, right);
            return .{ .num = checked.left / checked.right };
        },
        .STAR => {
            const checked = try state.checkNumberOperands(b.operator, left, right);
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
                                state.ctx.ally(),
                                "{s}{s}",
                                .{ ls.string, rs.string },
                            ),
                            .was_allocated = true,
                        },
                    },
                    else => {},
                },
                else => {},
            }

            state.ctx.report(b.operator, "Operands must be two numbers or two strings");
            return error.RuntimeError;
        },
        .GREATER => {
            const checked = try state.checkNumberOperands(b.operator, left, right);
            return .{ .boolean = checked.left > checked.right };
        },
        .GREATER_EQUAL => {
            const checked = try state.checkNumberOperands(b.operator, left, right);
            return .{ .boolean = checked.left >= checked.right };
        },
        .LESS => {
            const checked = try state.checkNumberOperands(b.operator, left, right);
            return .{ .boolean = checked.left < checked.right };
        },
        .LESS_EQUAL => {
            const checked = try state.checkNumberOperands(b.operator, left, right);
            return .{ .boolean = checked.left <= checked.right };
        },

        // unknown op
        else => return .{ .nil = {} },
    }
}

fn checkNumberOperands(
    state: *State,
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
    state.ctx.report(op, "Operands must be numbers");
    return error.RuntimeError;
}

pub inline fn visitExpr(state: *State, e: ast.Expr) Result {
    return try switch (e) {
        .binary => |b| state.visitBinary(b),
        .literal => |l| state.visitLiteral(l),
        .unary => |u| state.visitUnary(u),
        .@"var" => |v| state.visitVar(v),
        .lambda => |l| state.visitLambda(l),
        .assign => |a| state.visitAssign(a),
        .call => |c| state.visitCall(c),
        .this => |t| state.visitThis(t),
        .super => |s| state.visitSuper(s),
        .get => |g| state.visitGet(g),
        .set => |s| state.visitSet(s),
    };
}

inline fn visitStmt(state: *State, stmt: ast.Stmt) VoidResult {
    switch (stmt) {
        .expr => |e| {
            var v = try state.visitExpr(e);
            v.dispose(state);
        },
        .function => |f| try state.visitFunction(f),
        .@"if" => |i| try state.visitIf(i),
        .@"return" => |r| try state.visitReturn(r),
        .print => |p| try state.visitPrint(p),
        .@"var" => |v| try state.visitVarStmt(v),
        .@"while" => |w| try state.visitWhile(w),
        .block => |b| try state.visitBlock(b),
        .class => |c| try state.visitClass(c),
    }
}

pub inline fn indexFromEnv(state: *State, env: EnvHandle) []data.Value {
    return state.indexFromEnvp(state.envAt(env));
}
pub inline fn indexFromEnvp(state: *State, env: *Env) []data.Value {
    return state.values.items[env.values_begin..];
}

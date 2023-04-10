const std = @import("std");
const data = @import("data.zig");
const Env = @import("Env.zig");
const ast = @import("../ast.zig");
const context = @import("../context.zig");
const Ctx = @import("Ctx.zig");
const Token = @import("../Token.zig");
const AllocErr = std.mem.Allocator.Error;

const State = @This();
const Result = data.Result;
const VoidResult = data.AllocOrSignal!void;

const Local = struct {
    line: u32,
    col: u32,
    tok: Token.Ty,
};

fn local(token: Token) Local {
    return .{ .line = token.line, .col = token.col, .tok = token.ty };
}

// TODO: add a GPA with underlying arena {.underlying_allocator = arena} to the
// state, for the env stuff

globals: *Env,
current_env: *Env,
ctx: Ctx,
// Apparently it's an expr-to-integer map. WTF?
// We'll solve this properly by just storing its line,
// since there can only be one variable declaration in one line.
locals: std.AutoHashMapUnmanaged(Local, usize),
// TODO: we might want our own env pool, to be able to reuse memory. Not
// measured yet.
env_pool: std.heap.MemoryPool(Env),
// to be able to bind functions properly, we have to track instances
// apart
instance_pool: std.heap.MemoryPool(data.Instance),
class_pool: std.heap.MemoryPool(data.Class),
arena: std.heap.ArenaAllocator,
timer: std.time.Timer,
ret_val: ?data.Value = null,

pub fn init(gpa: std.mem.Allocator) !State {
    var env_pool = std.heap.MemoryPool(Env).init(gpa);
    errdefer env_pool.deinit();
    var arena_gpa = std.heap.ArenaAllocator.init(gpa);
    errdefer arena_gpa.deinit();

    const globals: *Env = try env_pool.create();
    globals.* = .{ .enclosing = null };
    try globals.define(
        arena_gpa.allocator(),
        "clock",
        .{ .callable = data.CallableVT{
            .ptr = undefined,
            .repr = "<native fn clock()>",
            .call = &getClock,
            .arity = 0,
        } },
    );

    return State{
        .globals = globals,
        .current_env = globals,
        .ctx = Ctx.init(gpa),
        .locals = .{},
        .env_pool = env_pool,
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
    state.instance_pool.deinit();
    state.class_pool.deinit();
    state.arena.deinit();
    state.ctx.deinit();
}

pub fn newEnv(state: *State, enclosing: ?*Env) AllocErr!*Env {
    const env: *Env = try state.env_pool.create();
    env.* = .{ .enclosing = enclosing };
    return env;
}

pub fn disposeEnv(state: *State, env: *Env) void {
    // Only dispose of values that have no other refs. This should clean up 90%
    // of the values in the scope, if not all.
    var it = env.values.valueIterator();
    while (it.next()) |v| if (v.depcount() == 0) v.dispose(state);
    // Dispose of the memory for the map too!
    env.values.deinit(state.arena.allocator());
    state.env_pool.destroy(env);
}

pub inline fn resolve(state: *State, at: Token, depth: usize) AllocErr!void {
    try state.locals.put(state.arena.allocator(), local(at), depth);
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
    const env: *Env = try state.newEnv(state.current_env);
    state.executeBlockIn(stmts, env) catch |err| {
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

fn getClock(_: *const anyopaque, state: *State, _: []const data.Value) Result {
    const conversion = comptime @intToFloat(f64, std.time.ns_per_s);
    const value = @intToFloat(f64, state.timer.read());
    return .{ .num = value / conversion };
}

const expr_vt = ast.Expr.VisitorVTable(Result, State){
    .visitBinary = visitBinary,
    .visitLiteral = visitLiteral,
    .visitUnary = visitUnary,
    .visitVar = visitVar,
    .visitLambda = visitLambda,
    .visitAssign = visitAssign,
    .visitCall = visitCall,
    .visitThis = visitThis,
    .visitSuper = visitSuper,
    .visitGet = visitGet,
    .visitSet = visitSet,
};
const stmt_vt = ast.Stmt.VisitorVTable(VoidResult, State){
    .visitExpr = visitExprStmt, // we have to convert to void
    .visitFunction = visitFunction,
    .visitIf = visitIf,
    .visitReturn = visitReturn,
    .visitPrint = visitPrint,
    .visitVar = visitVarStmt,
    .visitWhile = visitWhile,
    .visitBlock = visitBlock,
    .visitClass = visitClass,
};

fn visitClass(state: *State, stmt: ast.Stmt.Class) VoidResult {
    try state.current_env.define(
        state.arena.allocator(),
        stmt.name.lexeme,
        .{ .nil = {} },
    );

    var superclass: ?*data.Class = null;
    var class_env = state.current_env;
    if (stmt.superclass) |sp| {
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

        class_env = try state.newEnv(state.current_env);
        superc.refcount += 1;
        // It's ok to make a copy of the class info since they're immutable.
        try class_env.define(state.arena.allocator(), "super", .{ .class = superc });
        superclass = superc;
    }

    errdefer if (stmt.superclass) |_| state.disposeEnv(class_env);

    var methods: std.StringHashMapUnmanaged(data.Function) = .{};

    defer if (stmt.superclass) |_| if (methods.size == 0)
        state.disposeEnv(class_env);

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

    // This cannot fail with runtime error (or worse, return) because
    // we already defined it.
    state.current_env.assign(
        stmt.name,
        .{ .class = classp },
        &state.ctx,
    ) catch |err| return @errSetCast(std.mem.Allocator.Error, err);
}

fn visitBlock(state: *State, block: []const ast.Stmt) VoidResult {
    const env: *Env = try state.newEnv(state.current_env);
    defer state.disposeEnv(env);

    try state.executeBlockIn(block, env);
}

pub fn executeBlockIn(
    state: *State,
    block: []const ast.Stmt,
    in_env: *Env,
) VoidResult {
    const prev = state.current_env;
    defer state.current_env = prev;
    state.current_env = in_env;

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
    try state.current_env.define(state.arena.allocator(), v.name.lexeme, value);
}

fn visitPrint(state: *State, p: ast.Stmt.Print) VoidResult {
    const value = try state.visitExpr(p);
    std.debug.print("{s}\n", .{value});
}

fn visitReturn(state: *State, ret: ast.Stmt.Return) VoidResult {
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
    const function = data.Function{
        .decl = e.decl,
        .closure = state.current_env,
        .is_init = false,
    };
    try state.current_env.define(
        state.arena.allocator(),
        e.name.lexeme,
        .{ .func = function },
    );
}

fn visitExprStmt(state: *State, e: ast.Expr) VoidResult {
    _ = try state.visitExpr(e);
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
    const distance = state.locals.get(local(e.keyword)) orelse unreachable;
    const super: data.Value = state.current_env.getAt(distance, "super") orelse unreachable;
    const superclass: *const data.Class = switch (super) {
        .class => |c| c.superclass orelse unreachable,
        else => unreachable,
    };

    const this: data.Value = state.current_env.getAt(distance - 1, "this") orelse unreachable;

    const obj: *data.Instance = switch (this) {
        .instance => |i| i,
        else => unreachable,
    };

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
    state.disposeEnv(f.closure);
}

pub fn bind(
    state: *State,
    f: data.Function,
    instancep: *data.Instance,
) AllocErr!data.Function {
    const env: *Env = try state.newEnv(f.closure);
    errdefer state.disposeEnv(env);

    // this sucks, because now I have to make instances pointers. Not cool!
    try env.define(state.arena.allocator(), "this", .{ .instance = instancep });
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

    // use underlying GPA to be able to free the list, without messing with the arena.
    var arguments = std.ArrayList(data.Value).init(state.arena.allocator());
    defer arguments.deinit(); // they're getting copied anyway.

    for (call.arguments) |c| {
        try arguments.append(try state.visitExpr(c));
    }

    return try vt.call(vt.ptr, state, arguments.items);
}

fn visitAssign(state: *State, assign: ast.Expr.Assign) Result {
    var value = try state.visitExpr(assign.value.*);

    const distance = state.locals.get(local(assign.name));
    if (distance) |d| {
        try state.current_env.assignAt(
            state.arena.allocator(),
            d,
            assign.name.lexeme,
            value,
        );
    } else {
        try state.globals.assign(assign.name, value, &state.ctx);
    }

    return value;
}

fn visitLambda(state: *State, lam: ast.Expr.Lambda) Result {
    return .{ .func = data.Function{
        .decl = lam.decl,
        .closure = state.current_env,
        .is_init = false,
    } };
}

fn visitVar(state: *State, v: ast.Expr.Var) Result {
    return try state.lookupVariable(v);
}

fn lookupVariable(state: *State, v: Token) Result {
    const distance = state.locals.get(local(v));
    if (distance) |d| {
        return state.current_env.getAt(d, v.lexeme) orelse unreachable;
    } else {
        return try state.globals.get(v, &state.ctx);
    }
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

inline fn visitExpr(state: *State, e: ast.Expr) Result {
    return try e.accept(Result, State, expr_vt, state);
}

inline fn visitStmt(state: *State, e: ast.Stmt) VoidResult {
    return try e.accept(VoidResult, State, stmt_vt, state);
}

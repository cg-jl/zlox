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

pub const EnvHandle = usize;

const Local = packed struct {
    line: u16,
    col: u32,

    pub const U = std.meta.Int(.unsigned, @bitSizeOf(@This()));
};

const LocalContext = struct {
    pub fn eql(_: LocalContext, a: Local, b: Local) bool {
        return @bitCast(Local.U, a) == @bitCast(Local.U, b);
    }

    pub fn hash(_: LocalContext, k: Local) u64 {
        return @bitCast(Local.U, k);
    }
};

inline fn local(token: Token) Local {
    return .{
        .line = token.line,
        .col = token.col,
    };
}

const Depth = struct {
    env: usize,
    stack: u32,
};

const LocalMap = std.HashMapUnmanaged(
    Local,
    Depth,
    LocalContext,
    std.hash_map.default_max_load_percentage,
);

// TODO: add a GPA with underlying arena {.underlying_allocator = arena} to the
// state, for the env stuff

ctx: Ctx,
// Apparently it's an expr-to-integer map. WTF?
// We'll solve this properly by just storing its line,
// since there can only be one variable declaration in one line.
locals: LocalMap,
env_pool: std.ArrayListUnmanaged(Env),
// to be able to bind functions properly, we have to track instances
// apart
instance_pool: std.heap.MemoryPool(data.Instance),
class_pool: std.heap.MemoryPool(data.Class),
arena: std.heap.ArenaAllocator,
timer: std.time.Timer,
ret_val: ?data.Value = null,

pub fn init(gpa: std.mem.Allocator) !State {
    var arena_gpa = std.heap.ArenaAllocator.init(gpa);
    errdefer arena_gpa.deinit();

    var env_pool = try std.ArrayListUnmanaged(Env).initCapacity(arena_gpa.allocator(), 1);

    const globals: *Env = env_pool.addOneAssumeCapacity();
    globals.* = Env{ .enclosing = null };
    try globals.values.append(
        arena_gpa.allocator(),
        .{ .callable = data.CallableVT{
            .ptr = undefined,
            .repr = "<native fn clock()>",
            .call = &getClock,
            .arity = 0,
        } },
    );

    return State{
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
    state.instance_pool.deinit();
    state.class_pool.deinit();
    state.arena.deinit();
    state.ctx.deinit();
}

pub fn pushEnv(state: *State, enclosing: ?EnvHandle) AllocErr!EnvHandle {
    const env: *Env = try state.env_pool.addOne(state.arena.allocator());
    env.* = Env{ .enclosing = enclosing };
    return state.env_pool.items.len - 1;
}

// Clear environment, retaining old capacity.
pub fn clearEnv(state: *State, index: EnvHandle) void {
    const env: *Env = &state.env_pool.items[index];
    // Only dispose of values that have no other refs. This should clean up 90%
    // of the values in the scope, if not all.
    for (env.values.items) |*v| if (v.depcount() == 0) v.dispose(state);
    env.values.clearRetainingCapacity();
}

pub fn popEnv(state: *State) void {
    var env = state.env_pool.pop();
    // Dispose of the memory for the map too!
    env.values.deinit(state.arena.allocator());
}

pub inline fn resolve(state: *State, at: Token, env_depth: usize, stack_depth: u32) AllocErr!void {
    try state.locals.put(
        state.arena.allocator(),
        local(at),
        .{ .env = env_depth, .stack = stack_depth },
    );
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
    _ = try state.pushEnv(state.currentEnvHandle());
    defer state.popEnv();
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

fn getClock(_: *const anyopaque, state: *State, _: std.ArrayListUnmanaged(data.Value)) Result {
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
    const class_value_idx = @intCast(u32, state.currentEnv().values.items.len);
    try state.currentEnv().values.append(state.arena.allocator(), .{ .nil = {} });

    var superclass: ?*data.Class = null;
    var class_env = state.currentEnvHandle();
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

        class_env = try state.pushEnv(state.currentEnvHandle());
        superc.refcount += 1;
        // It's ok to make a copy of the class info since they're immutable.
        try state.envAt(class_env).values.append(state.arena.allocator(), .{ .class = superc });
        superclass = superc;
    }

    errdefer if (stmt.superclass) |_| state.popEnv();

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

    // We know it's at zero because it was the first thing that we pushed
    state.currentEnv().assignAt(
        0,
        class_value_idx,
        .{ .class = classp },
        state.env_pool.items,
    );
}

fn visitBlock(state: *State, block: []const ast.Stmt) VoidResult {
    _ = try state.pushEnv(state.currentEnvHandle());
    defer state.popEnv();

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

    try state.currentEnv().values.append(state.arena.allocator(), value);
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
        .closure = state.currentEnvHandle(),
        .is_init = false,
    };
    try state.currentEnv().values.append(state.arena.allocator(), .{ .func = function });
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
    const distance = state.locals.get(local(e.keyword)) orelse unreachable;
    const super: data.Value = state.currentEnv().getAt(distance.env, distance.stack, state.env_pool.items);
    const superclass: *const data.Class = switch (super) {
        .class => |c| c.superclass orelse unreachable,
        else => unreachable,
    };

    const this: data.Value = state.currentEnv().getAt(
        distance.env - 1,
        0,
        state.env_pool.items,
    );

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

pub fn unbind(state: *State, _: data.Function) void {
    state.popEnv();
}

pub fn bind(
    state: *State,
    f: data.Function,
    instancep: *data.Instance,
) AllocErr!data.Function {
    const env = try state.pushEnv(f.closure);
    errdefer state.popEnv();

    // this sucks, because now I have to make instances pointers. Not cool!
    try state.envAt(env).values.append(state.arena.allocator(), .{ .instance = instancep });
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

    // We create the arguments array with preallocated capacity, then move it into the callee so that they
    // can instantly use it as the environment
    var arguments = try std.ArrayListUnmanaged(data.Value).initCapacity(state.arena.allocator(), call.arguments.len);
    arguments.items.len = call.arguments.len;

    for (call.arguments, arguments.items) |c, *a| {
        a.* = try state.visitExpr(c);
    }

    return try vt.call(vt.ptr, state, arguments);
}

fn visitAssign(state: *State, assign: ast.Expr.Assign) Result {
    var value = try state.visitExpr(assign.value.*);

    const distance: Depth = state.locals.get(local(assign.name)) orelse @panic("Unresolved variable");
    state.currentEnv().assignAt(
        distance.env,
        distance.stack,
        value,
        state.env_pool.items,
    );

    return value;
}

fn visitLambda(state: *State, lam: ast.Expr.Lambda) Result {
    return .{ .func = data.Function{
        .decl = lam.decl,
        .closure = state.currentEnvHandle(),
        .is_init = false,
    } };
}

fn visitVar(state: *State, v: ast.Expr.Var) Result {
    return try state.lookupVariable(v);
}

/// Returns a pointer to the given environment.
/// This pointer can be invalidated anywhere that a new environment is pushed,
/// so it is advised to store the handle and call `envAt` for self-contained operations.
/// The function call is inexpensive; it just indexes a linear array.
pub inline fn envAt(state: *State, handle: EnvHandle) *Env {
    return &state.env_pool.items[handle];
}

pub inline fn currentEnv(state: *State) *Env {
    return state.envAt(state.currentEnvHandle());
}

pub inline fn currentEnvHandle(state: *const State) EnvHandle {
    return state.env_pool.items.len - 1;
}

fn lookupVariable(state: *State, v: Token) Result {
    const distance: Depth = state.locals.get(local(v)) orelse @panic("unresolved variable");
    const env: *Env = state.currentEnv().ancestor(distance.env, state.env_pool.items) orelse @panic("must have env at distance");
    // NOTE: from perf reports, this is where the most amount of cache misses
    // occur during execution. Looks like accessing the variable 'n' still makes
    // us miss a ton on performance.
    return env.values.items[distance.stack];
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

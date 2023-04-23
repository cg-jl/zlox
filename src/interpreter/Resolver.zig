const Resolver = @This();
const State = @import("State.zig");
const std = @import("std");
const context = @import("../context.zig");
const data = @import("data.zig");

const ast = @import("../ast.zig");

const VarInfo = packed struct {
    initialized: bool,
    stack_index: u32,
};

const Scope = std.StringHashMapUnmanaged(VarInfo);

interpreter: *State,
scopes: std.ArrayListUnmanaged(Scope) = .{},
current_function: FuncType = FuncType.none,
current_class: ClassType = ClassType.none,
arena: std.heap.ArenaAllocator,

pub fn init(interpreter: *State, alloc: std.mem.Allocator) !Resolver {
    var res = Resolver{
        .interpreter = interpreter,
        .arena = std.heap.ArenaAllocator.init(alloc),
    };
    // global scope
    try res.beginScope();

    try res.defineName("clock");
    return res;
}

pub fn deinit(r: *Resolver) void {
    r.arena.deinit();
}

const ClassType = enum(u2) { none, class, subclass };
const FuncType = enum(u2) { none, function, initializer, method };

const Result = data.AllocErr!void;

const expr_vt = ast.Expr.VisitorVTable(Result, Resolver){
    .visitVar = resolveVar,
    .visitUnary = resolveUnary,
    .visitThis = resolveThis,
    .visitSuper = resolveSuper,
    .visitSet = resolveSet,
    .visitBinary = resolveBinary,
    .visitLiteral = resolveLiteral,
    .visitGet = resolveGet,
    .visitCall = resolveCall,
    .visitAssign = resolveAssign,
    .visitLambda = resolveLambda,
};
const stmt_vt = ast.Stmt.VisitorVTable(Result, Resolver){
    .visitExpr = resolveExpr,
    .visitBlock = resolveBlock,
    .visitWhile = resolveWhile,
    .visitVar = resolveVarSt,
    .visitReturn = resolveReturn,
    .visitPrint = resolvePrint,
    .visitIf = resolveIf,
    .visitFunction = resolveFunction,
    .visitClass = resolveClass,
};

fn resolveInnerClass(r: *Resolver, methods: []const ast.Stmt.Function) Result {
    try r.beginScope();
    defer r.endScope();
    try r.defineName("this");
    for (methods) |method| {
        const is_init = std.mem.eql(u8, "init", method.name.lexeme);
        const decltype: FuncType = if (is_init) .initializer else .method;
        try r.resolveFuncDecl(method.decl, decltype);
    }
}

fn resolveClass(r: *Resolver, class: ast.Stmt.Class) Result {
    try r.declare(class.name);
    try r.define(class.name);

    const enclosing_class = r.current_class;
    defer r.current_class = enclosing_class;
    r.current_class = .class;

    if (class.superclass) |_| {
        r.current_class = .subclass;
        try r.beginScope();
        defer r.endScope();
        try r.defineName("super");
        try r.resolveInnerClass(class.methods);
    } else {
        try r.resolveInnerClass(class.methods);
    }
}

fn resolveFunction(r: *Resolver, func: ast.Stmt.Function) Result {
    try r.declare(func.name);
    try r.define(func.name);

    try r.resolveFuncDecl(func.decl, .function);
}

fn resolveIf(r: *Resolver, iff: ast.Stmt.If) Result {
    try r.resolveExpr(iff.condition);
    try r.resolveStmt(iff.then_branch.*);
    if (iff.else_branch) |else_branch| {
        try r.resolveStmt(else_branch.*);
    }
}

fn resolvePrint(r: *Resolver, print: ast.Stmt.Print) Result {
    try r.resolveExpr(print);
}

fn resolveReturn(r: *Resolver, ret: ast.Stmt.Return) Result {
    if (r.current_function == .none) {
        context.reportToken(ret.keyword, "Cannot return from top-level code");
    }
    if (ret.value == .literal and ret.value.literal == .nil) {
        if (r.current_function == .initializer) {
            context.reportToken(ret.keyword, "Cannot return a value from an initializer");
        }
    }
    try r.resolveExpr(ret.value);
}

fn resolveVarSt(r: *Resolver, v: ast.Stmt.Var) Result {
    try r.declare(v.name);
    if (v.init) |i| {
        try r.resolveExpr(i);
    }
    try r.define(v.name);
}

fn resolveWhile(r: *Resolver, wh: ast.Stmt.While) Result {
    try r.resolveExpr(wh.condition);
    try r.resolveStmt(wh.body.*);
}

fn resolveAssign(r: *Resolver, assign: ast.Expr.Assign) Result {
    try r.resolveExpr(assign.value.*);
    try r.resolveLocal(assign.name);
}

fn resolveCall(r: *Resolver, call: ast.Expr.Call) Result {
    try r.resolveExpr(call.callee.*);
    for (call.arguments) |arg| {
        try r.resolveExpr(arg);
    }
}

fn resolveGet(r: *Resolver, g: ast.Expr.Get) Result {
    try r.resolveExpr(g.obj.*);
}

fn resolveLiteral(_: *Resolver, _: ast.Expr.Literal) Result {}

fn resolveBinary(r: *Resolver, s: ast.Expr.Binary) Result {
    try r.resolveExpr(s.left.*);
    try r.resolveExpr(s.right.*);
}

fn resolveSet(r: *Resolver, s: ast.Expr.Set) Result {
    try r.resolveExpr(s.value.*);
    try r.resolveExpr(s.obj.*);
}

fn resolveSuper(r: *Resolver, s: ast.Expr.Super) Result {
    if (r.current_class == .none) {
        context.reportToken(s.keyword, "Cannot use 'super' outside of a class");
    } else if (r.current_class != .subclass) {
        context.reportToken(s.keyword, "Cannot use 'super' in a class with no superclass");
    } else {
        try r.resolveLocal(s.keyword);
    }
}

fn resolveThis(r: *Resolver, th: ast.Expr.Var) Result {
    if (r.current_class == .none) {
        context.reportToken(th, "Cannot use 'this' outside of a class");
    } else {
        try r.resolveLocal(th);
    }
}

fn resolveUnary(r: *Resolver, u: ast.Expr.Unary) Result {
    try r.resolveExpr(u.right.*);
}

fn resolveVar(r: *Resolver, name: ast.Expr.Var) Result {
    if (r.latestScope()) |scope| {
        if (scope.get(name.lexeme)) |x| {
            if (!x.initialized) {
                context.reportToken(name, "Cannot read local variable in its own initializer");
            }
        }
    }

    try r.resolveLocal(name);
}

fn resolveLambda(r: *Resolver, l: ast.Expr.Lambda) Result {
    try r.resolveFuncDecl(l.decl, .function);
}

fn resolveFuncDecl(r: *Resolver, func: ast.FuncDecl, ty: FuncType) Result {
    const enclosing_function = r.current_function;
    defer r.current_function = enclosing_function;
    r.current_function = ty;

    try r.beginScope();
    defer r.endScope();

    for (func.params) |param| {
        try r.declare(param);
        try r.define(param);
    }

    // Do not create a new scope for these. This makes shadowing arguments
    // illegal, but they're mutable so it's OK.
    for (func.body) |s| try r.resolveStmt(s);
}

pub fn resolveBlock(r: *Resolver, block: []const ast.Stmt) Result {
    try r.beginScope();
    defer r.endScope();
    for (block) |s| try r.resolveStmt(s);
}

fn latestScope(r: *Resolver) ?*Scope {
    if (r.scopes.items.len == 0) return null;
    return &r.scopes.items[r.scopes.items.len - 1];
}

fn declare(r: *Resolver, name: ast.Expr.Var) Result {
    const scope = r.latestScope() orelse return;

    const get_or_put: Scope.GetOrPutResult = try scope.getOrPut(r.ally(), name.lexeme);
    if (get_or_put.found_existing) {
        context.reportToken(
            name,
            "Variable with this name already declared in this scope.",
        );
    }

    get_or_put.value_ptr.initialized = false;
    get_or_put.value_ptr.stack_index = scope.size - 1;
}

fn defineName(r: *Resolver, name: []const u8) data.AllocErr!void {
    const scope = r.latestScope() orelse @panic("should have scope to define");
    const get_or_put: Scope.GetOrPutResult = try scope.getOrPut(r.ally(), name);
    if (!get_or_put.found_existing) {
        get_or_put.value_ptr.stack_index = scope.size - 1;
    }
    get_or_put.value_ptr.initialized = true;
}

fn define(r: *Resolver, name: ast.Expr.Var) data.AllocErr!void {
    try r.defineName(name.lexeme);
}

fn resolveLocal(r: *Resolver, name: ast.Expr.Var) data.AllocErr!void {
    var i: usize = r.scopes.items.len;
    while (i > 0) : (i -= 1) {
        const scope: *const Scope = &r.scopes.items[i - 1];
        if (scope.get(name.lexeme)) |info| {
            try r.interpreter.resolve(name, r.scopes.items.len - i, info.stack_index);
            return;
        }
    }

    context.reportToken(name, "Could not resolve variable");
}

fn beginScope(r: *Resolver) Result {
    try r.scopes.append(r.ally(), .{});
}

fn endScope(r: *Resolver) void {
    var env = r.scopes.pop();
    env.deinit(r.ally());
}

pub fn resolveExpr(r: *Resolver, e: ast.Expr) Result {
    try e.accept(Result, Resolver, expr_vt, r);
}

pub fn resolveStmt(r: *Resolver, s: ast.Stmt) Result {
    try s.accept(Result, Resolver, stmt_vt, r);
}

inline fn ally(r: *Resolver) std.mem.Allocator {
    return r.arena.allocator();
}

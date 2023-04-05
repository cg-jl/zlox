const ast = @import("../ast.zig");
const std = @import("std");

const Printer = @This();

indent_level: usize = 0,

const expr_vt = ast.Expr.VisitorVTable(void, Printer){
    .visitBinary = printBinary,
    .visitLiteral = printLiteral,
    .visitUnary = printUnary,
    .visitVar = printVar,
    .visitLambda = printLambda,
    .visitAssign = printAssign,
    .visitCall = printCall,
    .visitThis = printThis,
    .visitSuper = printSuper,
    .visitGet = printGet,
    .visitSet = printSet,
};

const stmt_vt = ast.Stmt.VisitorVTable(void, Printer){
    .visitExpr = printExpr,
    .visitFunction = printFunction,
    .visitIf = printIf,
    .visitReturn = printReturn,
    .visitPrint = printPrint,
    .visitVar = printVarSt,
    .visitWhile = printWhile,
    .visitBlock = printBlock,
    .visitClass = printClass,
};

fn printClass(p: *Printer, class: ast.Stmt.Class) void {
    std.debug.print("Class{{\n", .{});
    const current_ident = p.indent_level;
    defer {
        p.indent_level = current_ident;
        p.makeIndent();
        std.debug.print("}}\n", .{});
    }

    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("name = {s}\n", .{class.name.lexeme});
    p.makeIndent();
    if (class.methods.len == 0) {
        std.debug.print("(no methods)\n", .{});
    } else {
        std.debug.print("methods = [\n", .{});
        const pre_methods_level = p.indent_level;
        defer {
            p.indent_level = pre_methods_level;
            p.makeIndent();
            std.debug.print("]\n", .{});
        }
        p.indent_level += 1;

        for (class.methods) |m| {
            p.makeIndent();
            p.printFunction(m);
        }
    }

    if (class.superclass) |s| {
        p.makeIndent();
        std.debug.print("superclass = {s}\n", .{s.lexeme});
    }
}

fn printBlock(p: *Printer, body: []const ast.Stmt) void {
    if (body.len == 0) {
        std.debug.print("Empty Block\n", .{});
        return;
    }

    std.debug.print("Block{{\n", .{});
    const current_ident = p.indent_level;
    defer {
        p.indent_level = current_ident;
        p.makeIndent();
        std.debug.print("}}\n", .{});
    }

    p.indent_level += 1;
    for (body) |st| {
        p.makeIndent();
        p.printStmt(st);
    }
}

fn printWhile(p: *Printer, wh: ast.Stmt.While) void {
    std.debug.print("While{{\n", .{});
    const current_ident = p.indent_level;
    defer {
        p.indent_level = current_ident;
        p.makeIndent();
        std.debug.print("}}\n", .{});
    }

    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("condition = ", .{});
    p.printExpr(wh.condition);
    p.makeIndent();
    std.debug.print("body = ", .{});
    p.printStmt(wh.body.*);
}

fn printVarSt(p: *Printer, v: ast.Stmt.Var) void {
    std.debug.print("VarDecl{{\n", .{});
    const current_ident = p.indent_level;
    defer {
        p.indent_level = current_ident;
        p.makeIndent();
        std.debug.print("}}\n", .{});
    }

    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("name = {s}\n", .{v.name.lexeme});
    if (v.init) |init| {
        p.makeIndent();
        std.debug.print("init = ", .{});
        p.printExpr(init);
    }
}

fn printPrint(p: *Printer, pr: ast.Stmt.Print) void {
    std.debug.print("Print(\n", .{});
    const current_ident = p.indent_level;
    defer {
        p.indent_level = current_ident;
        p.makeIndent();
        std.debug.print(")\n", .{});
    }
    p.indent_level += 1;
    p.makeIndent();
    p.printExpr(pr);
}

fn printReturn(p: *Printer, ret: ast.Stmt.Return) void {
    std.debug.print("Return(\n", .{});
    const current_ident = p.indent_level;
    defer {
        p.indent_level = current_ident;
        p.makeIndent();
        std.debug.print(")\n", .{});
    }
    p.indent_level += 1;
    p.makeIndent();
    p.printExpr(ret.value);
}

fn printIf(p: *Printer, iff: ast.Stmt.If) void {
    std.debug.print("If{{\n", .{});
    const current_ident = p.indent_level;
    defer {
        p.indent_level = current_ident;
        p.makeIndent();
        std.debug.print("}}\n", .{});
    }

    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("condition = ", .{});
    p.printExpr(iff.condition);
    p.makeIndent();
    std.debug.print("then_branch = ", .{});
    p.printStmt(iff.then_branch.*);
    if (iff.else_branch) |else_branch| {
        p.makeIndent();
        std.debug.print("else_branch = ", .{});
        p.printStmt(else_branch.*);
    }
}

fn printFunction(p: *Printer, func: ast.Stmt.Function) void {
    std.debug.print("Function{{\n", .{});
    const current_ident = p.indent_level;
    defer {
        p.indent_level = current_ident;
        p.makeIndent();
        std.debug.print("}}\n", .{});
    }

    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("name = {s}\n", .{func.name.lexeme});
    p.makeIndent();
    std.debug.print("decl = ", .{});
    p.printFuncDecl(func.decl);
}

fn printSet(p: *Printer, set: ast.Expr.Set) void {
    std.debug.print("Set{{\n", .{});
    const current_ident = p.indent_level;
    defer {
        p.indent_level = current_ident;
        p.makeIndent();
        std.debug.print("}}\n", .{});
    }
    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("obj: ", .{});
    p.printExpr(set.obj.*);
    p.makeIndent();
    std.debug.print("name: {s}\n", .{set.name.lexeme});
    p.makeIndent();
    std.debug.print("value: ", .{});
    p.printExpr(set.value.*);
}

fn printGet(p: *Printer, get: ast.Expr.Get) void {
    std.debug.print("Get{{\n", .{});
    const current_ident = p.indent_level;
    defer {
        p.indent_level = current_ident;
        p.makeIndent();
        std.debug.print("}}\n", .{});
    }
    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("obj: ", .{});
    p.printExpr(get.obj.*);
    p.makeIndent();
    std.debug.print("name: {s}\n", .{get.name.lexeme});
}

fn printSuper(_: *Printer, super: ast.Expr.Super) void {
    std.debug.print("Super{s}\n", .{super.method.lexeme});
}

fn printThis(_: *Printer, _: ast.Expr.Var) void {
    std.debug.print("This\n", .{});
}

fn printCall(p: *Printer, call: ast.Expr.Call) void {
    std.debug.print("Call{{\n", .{});
    const current_ident = p.indent_level;
    defer {
        p.indent_level = current_ident;
        p.makeIndent();
        std.debug.print("}}\n", .{});
    }
    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("callee: ", .{});
    p.printExpr(call.callee.*);
    p.makeIndent();
    if (call.arguments.len == 0) {
        std.debug.print("arguments: []\n", .{});
    } else {
        std.debug.print("arguments: [\n", .{});
        const pre_arg_ident = p.indent_level;
        defer {
            p.indent_level = pre_arg_ident;
            p.makeIndent();
            std.debug.print("]\n", .{});
        }
        p.indent_level += 1;
        for (call.arguments) |arg| {
            p.makeIndent();
            p.printExpr(arg);
        }
    }
}

fn printAssign(p: *Printer, assign: ast.Expr.Assign) void {
    std.debug.print("Assign{{\n", .{});
    const current_ident = p.indent_level;
    defer {
        p.indent_level = current_ident;
        p.makeIndent();
        std.debug.print("}}\n", .{});
    }
    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("name: {s}\n", .{assign.name.lexeme});
    p.makeIndent();
    std.debug.print("value: ", .{});
    p.printExpr(assign.value.*);
}

fn printLambda(p: *Printer, v: ast.Expr.Lambda) void {
    std.debug.print("Lambda: ", .{});
    p.printFuncDecl(v.decl);
}

fn printVar(_: *Printer, v: ast.Expr.Var) void {
    std.debug.print("Var({s})\n", .{v.lexeme});
}

fn printUnary(p: *Printer, u: ast.Expr.Unary) void {
    std.debug.print("Unary{{\n", .{});
    const current_ident = p.indent_level;
    defer {
        p.indent_level = current_ident;
        p.makeIndent();
        std.debug.print("}}\n", .{});
    }
    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("op: {}\n", .{u.operator});
    p.makeIndent();
    std.debug.print("right: ", .{});
    p.printExpr(u.right.*);
}

fn printLiteral(_: *Printer, t: ast.Expr.Literal) void {
    std.debug.print("Literal({})\n", .{t});
}

fn printBinary(p: *Printer, b: ast.Expr.Binary) void {
    std.debug.print("Binary{{\n", .{});
    const current_ident = p.indent_level;
    defer {
        p.indent_level = current_ident;
        p.makeIndent();
        std.debug.print("}}\n", .{});
    }
    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("left: ", .{});
    p.printExpr(b.left.*);
    p.makeIndent();
    std.debug.print("op: {}\n", .{b.operator});
    p.makeIndent();
    std.debug.print("right: ", .{});
    p.printExpr(b.right.*);
}

pub fn printExpr(p: *Printer, e: ast.Expr) void {
    e.accept(void, Printer, expr_vt, p);
}

pub fn printStmt(p: *Printer, stmt: ast.Stmt) void {
    stmt.accept(void, Printer, stmt_vt, p);
}

fn makeIndent(p: *const Printer) void {
    var i: usize = 0;
    while (i < p.indent_level) : (i += 1) {
        std.debug.print("  ", .{});
    }
}

fn printFuncDecl(p: *Printer, decl: ast.FuncDecl) void {
    std.debug.print("FuncDecl{{\n", .{});
    const current_ident = p.indent_level;
    defer {
        p.indent_level = current_ident;
        p.makeIndent();
        std.debug.print("}}\n", .{});
    }
    p.indent_level += 1;
    p.makeIndent();
    if (decl.name) |name| {
        std.debug.print("name = \"{s}\"\n", .{name.lexeme});
    } else {
        std.debug.print("(unnamed)\n", .{});
    }
    p.makeIndent();
    std.debug.print("params: [", .{});
    if (decl.params.len > 0) {
        std.debug.print("{s}", .{decl.params[0].lexeme});
        for (decl.params[1..]) |param| {
            std.debug.print(" {s}", .{param.lexeme});
        }
    }
    std.debug.print("]\n", .{});
    p.makeIndent();
    std.debug.print("body = ", .{});
    p.printBlock(decl.body);
}

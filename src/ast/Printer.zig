const Ast = @import("Ast.zig");
const std = @import("std");
const Token = @import("../Token.zig");

const Printer = @This();

indent_level: usize = 0,
ast: Ast,

pub fn printNode(p: *Printer, node_index: Ast.Index) void {
    const tag = p.ast.nodes.items(.tag)[node_index];
    switch (tag) {
        .print => p.printPrint(p.ast.unpack(Ast.Print, node_index)),
        .ret => p.printReturn(p.ast.unpack(Ast.Return, node_index)),
        .naked_ret => p.printReturn(null),
        .init_var_decl => {
            const info = p.ast.unpack(Ast.InitVarDecl, node_index);
            p.printVarSt(info.name, info.init);
        },
        .naked_var_decl => p.printVarSt(
            p.ast.unpack(Ast.NakedVarDecl, node_index),
            null,
        ),
        .naked_while => p.printWhile(
            null,
            p.ast.unpack(Ast.NakedWhile, node_index),
        ),
        .@"while" => {
            const info = p.ast.unpack(Ast.While, node_index);
            p.printWhile(info.cond, info.body);
        },
        .block => p.printBlock(p.ast.unpack(Ast.Block, node_index)),
        .single_class => {
            const info = p.ast.unpack(Ast.SingleClass, node_index);
            p.printClass(info.name, info.methods, null);
        },
        .class => {
            const info = p.ast.unpack(Ast.FullClass, node_index);
            p.printClass(info.name, info.methods, info.superclass);
        },
        .if_simple => {
            const info = p.ast.unpack(Ast.IfSimple, node_index);
            p.printIf(info.cond, info.then_branch, null);
        },
        .@"if" => {
            const info = p.ast.unpack(Ast.FullIf, node_index);
            p.printIf(info.cond, info.then_branch, info.else_branch);
        },
        .function => p.printFunction(p.ast.unpack(Ast.Function, node_index)),
        .set => p.printSet(p.ast.unpack(Ast.Set, node_index)),
        .get => p.printGet(p.ast.unpack(Ast.Get, node_index)),
        .super => std.debug.print("Super.{s}\n", .{
            p.ast.unpack(Ast.Super, node_index).lexeme,
        }),
        .this => std.debug.print("This\n", .{}),
        .call => p.printCall(p.ast.unpack(Ast.Call, node_index)),
        .assign => p.printAssign(p.ast.unpack(Ast.Assign, node_index)),
        .lambda => p.printLambda(p.ast.unpack(Ast.Function, node_index)),
        .fetchVar => p.printVar(p.ast.unpack(Ast.FetchVar, node_index)),
        .unary => p.printUnary(p.ast.unpack(Ast.Unary, node_index)),
        .literal => p.printLiteral(
            p.ast.unpack(Ast.Literal, node_index).extractLiteral(),
        ),
        .binary => p.printBinary(p.ast.unpack(Ast.Binary, node_index)),
    }
}

fn printClass(
    p: *Printer,
    name: Token,
    methods: Ast.SliceIndex,
    superclass: ?Token,
) void {
    std.debug.print("Class:\n", .{});
    const current_ident = p.indent_level;
    defer p.indent_level = current_ident;

    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("name = {s}\n", .{name.lexeme});
    p.makeIndent();
    if (methods.len() == 0) {
        std.debug.print("(no methods)\n", .{});
    } else {
        std.debug.print("methods:\n", .{});
        const pre_methods_level = p.indent_level;
        defer p.indent_level = pre_methods_level;

        p.indent_level += 1;

        for (methods.start..methods.end) |i| {
            p.makeIndent();
            std.debug.assert(p.ast.nodes.items(.tag)[i] == .function);
            p.printFunction(p.ast.unpack(Ast.Function, i));
        }
    }

    if (superclass) |s| {
        p.makeIndent();
        std.debug.print("superclass = {s}\n", .{s.lexeme});
    }
}

fn printBlock(p: *Printer, body: Ast.SliceIndex) void {
    if (body.len() == 0) {
        std.debug.print("Empty Block\n", .{});
        return;
    }

    std.debug.print("Block:\n", .{});
    const current_ident = p.indent_level;
    defer p.indent_level = current_ident;

    p.indent_level += 1;
    for (body.start..body.end) |st| {
        p.makeIndent();
        p.printNode(@intCast(Ast.Index, st));
    }
}

fn printWhile(
    p: *Printer,
    cond: ?Ast.Index,
    body: Ast.Index,
) void {
    std.debug.print("While:\n", .{});
    const current_ident = p.indent_level;
    defer p.indent_level = current_ident;

    p.indent_level += 1;
    if (cond) |condition| {
        p.makeIndent();
        std.debug.print("condition = ", .{});
        p.printNode(condition);
    }
    p.makeIndent();
    std.debug.print("body = ", .{});
    p.printNode(body);
}

fn printVarSt(
    p: *Printer,
    name: Token,
    init: ?Ast.Index,
) void {
    std.debug.print("VarDecl:\n", .{});
    const current_ident = p.indent_level;
    defer p.indent_level = current_ident;

    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("name = {s}\n", .{name.lexeme});
    if (init) |init_node| {
        p.makeIndent();
        std.debug.print("init = ", .{});
        p.printNode(init_node);
    }
}

fn printPrint(p: *Printer, pr: Ast.Index) void {
    std.debug.print("Print: ", .{});
    const current_ident = p.indent_level;
    defer p.indent_level = current_ident;

    p.indent_level += 1;
    p.printNode(pr);
}

fn printReturn(p: *Printer, ret: ?Ast.Index) void {
    std.debug.print("Return:\n", .{});
    const current_ident = p.indent_level;
    defer p.indent_level = current_ident;

    if (ret) |value| {
        p.indent_level += 1;
        p.makeIndent();
        p.printNode(value);
    }
}

fn printIf(
    p: *Printer,
    cond: Ast.Index,
    then_branch: Ast.Index,
    else_branch: ?Ast.Index,
) void {
    std.debug.print("If:\n", .{});
    const current_ident = p.indent_level;
    defer p.indent_level = current_ident;

    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("condition = ", .{});
    p.printNode(cond);
    p.makeIndent();
    std.debug.print("then_branch = ", .{});
    p.printNode(then_branch);
    if (else_branch) |else_branch_index| {
        p.makeIndent();
        std.debug.print("else_branch = ", .{});
        p.printNode(else_branch_index);
    }
}

fn printFunction(p: *Printer, func: Ast.Function) void {
    std.debug.print("Function:\n", .{});
    const current_ident = p.indent_level;
    defer p.indent_level = current_ident;

    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("name = {s}\n", .{func.name.lexeme});
    p.makeIndent();
    std.debug.print("decl = ", .{});
    p.printFuncDecl(func.decl);
}

fn printSet(p: *Printer, set: Ast.Set) void {
    std.debug.print("Set:\n", .{});
    const current_ident = p.indent_level;
    defer p.indent_level = current_ident;

    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("obj: ", .{});
    p.printNode(set.obj);
    p.makeIndent();
    std.debug.print("name: {s}\n", .{set.name.lexeme});
    p.makeIndent();
    std.debug.print("value: ", .{});
    p.printNode(set.value);
}

fn printGet(p: *Printer, get: Ast.Get) void {
    std.debug.print("Get:\n", .{});
    const current_ident = p.indent_level;
    defer p.indent_level = current_ident;

    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("obj: ", .{});
    p.printNode(get.obj);
    p.makeIndent();
    std.debug.print("name: {s}\n", .{get.name.lexeme});
}

fn printCall(p: *Printer, call: Ast.Call) void {
    std.debug.print("Call:\n", .{});
    const current_ident = p.indent_level;
    defer p.indent_level = current_ident;

    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("callee: ", .{});
    p.printNode(call.callee);
    if (call.params.len() != 0) {
        p.makeIndent();
        std.debug.print("arguments:\n", .{});
        const pre_arg_ident = p.indent_level;
        defer p.indent_level = pre_arg_ident;

        p.indent_level += 1;
        for (call.params.start..call.params.end) |arg| {
            p.makeIndent();
            p.printNode(@intCast(Ast.Index, arg));
        }
    }
}

fn printAssign(p: *Printer, assign: Ast.Assign) void {
    std.debug.print("Assign:\n", .{});
    const current_ident = p.indent_level;
    defer p.indent_level = current_ident;

    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("name: {s}\n", .{assign.name.lexeme});
    p.makeIndent();
    std.debug.print("value: ", .{});
    p.printNode(assign.rhs);
}

fn printLambda(p: *Printer, v: Ast.Function) void {
    std.debug.print("Lambda: ", .{});
    p.printFuncDecl(v.decl);
}

fn printVar(_: *Printer, v: Token) void {
    std.debug.print("Var({s})\n", .{v.lexeme});
}

fn printUnary(p: *Printer, u: Ast.Unary) void {
    std.debug.print("Unary:\n", .{});
    const current_ident = p.indent_level;
    defer p.indent_level = current_ident;

    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("op: {s}\n", .{u.op.lexeme});
    p.makeIndent();
    std.debug.print("right: ", .{});
    p.printNode(u.rhs);
}

fn printLiteral(_: *Printer, t: Token.TaggedLiteral) void {
    switch (t) {
        .string => |s| std.debug.print("Literal(\"{s}\")\n", .{s}),
        else => std.debug.print("Literal({})\n", .{t}),
    }
}

fn printBinary(p: *Printer, b: Ast.Binary) void {
    std.debug.print("Binary:\n", .{});
    const current_ident = p.indent_level;
    defer p.indent_level = current_ident;

    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("left: ", .{});
    p.printNode(b.lhs);
    p.makeIndent();
    std.debug.print("op: {s}\n", .{b.op.lexeme});
    p.makeIndent();
    std.debug.print("right: ", .{});
    p.printNode(b.rhs);
}

fn makeIndent(p: *const Printer) void {
    var i: usize = 0;
    while (i < p.indent_level) : (i += 1) {
        std.debug.print("  ", .{});
    }
}

fn printFuncDecl(p: *Printer, decl: Ast.Node.FuncDecl) void {
    std.debug.print("FuncDecl:\n", .{});
    const current_ident = p.indent_level;
    defer p.indent_level = current_ident;
    p.indent_level += 1;
    p.makeIndent();
    std.debug.print("params: [", .{});
    if (decl.params.len() > 0) {
        std.debug.print("{s}", .{p.ast.tokens[decl.params.start].lexeme});
        for (decl.params.start + 1..decl.params.end) |param| {
            std.debug.print(" {s}", .{p.ast.tokens[param].lexeme});
        }
    }
    std.debug.print("]\n", .{});
    p.makeIndent();
    std.debug.print("body = ", .{});
    p.printBlock(decl.body);
}

const NodeParser = @This();
const Ast = @import("ast/Ast.zig");
const NodeBuilder = @import("ast/NodeBuilder.zig");
const std = @import("std");
const context = @import("context.zig");
const Token = @import("Token.zig");

// TODO: use a Pratt parser and compare timings.

const AllocErr = std.mem.Allocator.Error;
const ParseErr = error{ParseError};
const AnyErr = NodeBuilder.Error || ParseErr;

tokens: []const Token,
current: usize = 0,
builder: *NodeBuilder,
/// An allocator for temporary storage of AST nodes, before inserting them into
/// the AST structure, to ensure that they are contiguous. No memory is leaked
/// into it.
temp_node_allocator: std.mem.Allocator,

// TODO: logic and & logic or should have different nodes for their runtime
// capabilities.

pub inline fn ast(p: *const NodeParser) Ast {
    return Ast{
        .extra_data = p.builder.extra_data.items,
        .nodes = p.builder.node_list.slice(),
        .tokens = p.tokens,
    };
}

pub fn parse(p: *NodeParser, root: *std.ArrayList(Ast.Index)) AllocErr!Ast {
    while (!p.isAtEnd()) {
        // I just ignore malformed statements for now. If I implement
        // recoveries from expr parser that yield 'garbage' marked expressions,
        // I'll rethink it. If so, I also have to distinguish the 'semicolon'
        // problem, where semicolons are enforced when the parser knows exactly
        // where it is.
        const decl = try p.tryDeclaration();
        if (decl) |ind| {
            try root.append(ind);
        }
    }

    return p.ast();
}

pub fn tryDeclaration(p: *NodeParser) AllocErr!?Ast.Index {
    return p.declaration(false) catch |err| {
        if (err == error.ParseError) {
            p.synchronize();
            return null;
        }
        return @errSetCast(AllocErr, err);
    };
}

fn declaration(p: *NodeParser, comptime required_semicolon: bool) AnyErr!Ast.Index {
    switch (p.advance().ty) {
        .CLASS => return p.classDecl(),
        .FUN => {
            if (p.check(.LEFT_PAREN)) {
                p.current -= 1;
                return p.exprStmt(required_semicolon); // will re-match on 'fun'
            } else {
                return p.function("function");
            }
        },
        .VAR => return p.varDecl(required_semicolon),
        else => {
            p.current -= 1;
            return p.statement(required_semicolon);
        },
    }
}

fn classDecl(p: *NodeParser) AnyErr!Ast.Index {
    const name = try p.consume(.IDENTIFIER, "Expected class name");
    if (p.match(.LESS)) |_| {
        const superclass = try p.consume(.IDENTIFIER, "Expected superclass name");
        var methods = try p.classMethods();
        defer methods.deinit();
        return p.builder.class(name, superclass, methods.items);
    } else {
        var methods = try p.classMethods();
        defer methods.deinit();
        return p.builder.singleClass(name, methods.items);
    }
}

fn classMethods(p: *NodeParser) AnyErr!std.ArrayList(Ast.Node) {
    _ = try p.consume(.LEFT_BRACE, "Expected '{' before class body");
    var methods = std.ArrayList(Ast.Node).init(p.temp_node_allocator);
    errdefer methods.deinit();

    while (!p.check(.RIGHT_BRACE) and !p.isAtEnd()) {
        _ = try p.function("method");
        try methods.append(p.builder.node_list.pop());
    }

    _ = try p.consume(.RIGHT_BRACE, "Expected '}' after class body");

    return methods;
}

fn function(p: *NodeParser, comptime kind: []const u8) AnyErr!Ast.Index {
    const name = try p.consume(.IDENTIFIER, "Expected " ++ kind ++ " name");
    const decl = try p.funcDecl(kind);
    return p.builder.function(name, decl);
}

fn statement(p: *NodeParser, comptime required_semicolon: bool) AnyErr!Ast.Index {
    return switch (p.advance().ty) {
        .FOR => p.forStmt(required_semicolon),
        .PRINT => p.printStmt(required_semicolon),
        .RETURN => p.returnStmt(),
        .LEFT_BRACE => p.block(),
        .IF => p.ifStmt(required_semicolon),
        .WHILE => p.whileStmt(required_semicolon),
        else => tryExpr: {
            p.current -= 1;
            break :tryExpr p.exprStmt(required_semicolon);
        },
    };
}

fn lambda(p: *NodeParser, kw: Token) AnyErr!Ast.Index {
    const decl = try p.funcDecl("lambda");
    return p.builder.function(kw, decl);
}

// NOTE: what about doing naked/one/two argument versions?
fn funcDecl(p: *NodeParser, comptime kind: []const u8) AnyErr!Ast.Node.FuncDecl {
    _ = try p.consume(.LEFT_PAREN, "Expected '(' after " ++ kind ++ " name");
    const params_begin = p.builder.stampTokenAnnotate();
    errdefer p.builder.dropTokenAnnotate(params_begin);

    if (!p.check(.RIGHT_PAREN)) {
        try p.builder.annotateToken(try p.consume(.IDENTIFIER, "Expected parameter name"));
        while (p.match(.COMMA)) |_| {
            if (p.builder.stampTokenAnnotate() - params_begin > 255) {
                p.report(p.peek(), "Can't have more than 255 parameters");
                return error.ParseError;
            }
            try p.builder.annotateToken(try p.consume(.IDENTIFIER, "Expected parameter name"));
        }
    }

    const params_end = p.builder.stampTokenAnnotate();

    _ = try p.consume(.RIGHT_PAREN, "Expected ')' after parameters");

    // Function body
    _ = try p.consume(.LEFT_BRACE, "Expected '{' before " ++ kind ++ " body");
    // we already know that we will interpret/come through a block, so
    // we only need the pushed nodes.
    const body: Ast.SliceIndex = grabBlockRange: {
        _ = try p.block();
        const block_node: Ast.Node = p.builder.node_list.pop();
        break :grabBlockRange block_node.data.toRange();
    };

    return Ast.Node.FuncDecl{
        .params = .{ .start = params_begin, .end = params_end },
        .body = body,
    };
}

fn forStmt(p: *NodeParser, comptime required_semicolon: bool) AnyErr!Ast.Index {
    _ = try p.consume(.LEFT_PAREN, "Expected '(' after 'for'");

    var init_stmt: ?Ast.Node = null;
    switch (p.peek().ty) {
        .SEMICOLON => {
            p.current += 1;
        },
        .VAR => {
            p.current += 1;
            _ = try p.varDecl(required_semicolon);
            init_stmt = p.builder.node_list.pop();
        },
        else => {
            _ = try p.exprStmt(required_semicolon);
            init_stmt = p.builder.node_list.pop();
        },
    }

    const cond = switch (p.peek().ty) {
        .SEMICOLON => null,
        else => try p.expression(),
    };

    _ = try p.consume(.SEMICOLON, "Expected ';' after loop condition");

    const incr = if (!p.check(.RIGHT_PAREN)) exprNode: {
        _ = try p.expression();
        break :exprNode p.builder.node_list.pop();
    } else null;

    _ = try p.consume(.RIGHT_PAREN, "Expected ')' after for clauses");

    var body = try p.statement(required_semicolon);

    // desugar
    if (incr) |i| {
        const body_node = p.builder.node_list.pop();
        body = try p.builder.block(&.{ body_node, i });
    }

    // make the while loop
    if (cond) |cind| {
        body = try p.builder.whileNode(cind, body);
    } else {
        body = try p.builder.nakedWhile(body);
    }

    // put the init statement node if needed.
    if (init_stmt) |snode| {
        const while_node = p.builder.node_list.pop();
        body = try p.builder.block(&.{ snode, while_node });
    }

    return body;
}

fn varDecl(p: *NodeParser, comptime required_semicolon: bool) AnyErr!Ast.Index {
    const name = try p.consume(.IDENTIFIER, "Expected variable name");
    var index: AnyErr!Ast.Index = undefined;
    if (p.match(.EQUAL)) |_| {
        const init = try p.expression();
        index = p.builder.initVarDecl(name, init);
    } else {
        index = p.builder.nakedVarDecl(name);
    }
    if (required_semicolon) {
        _ = try p.consume(.SEMICOLON, "Expected ';' after variable declaration");
    }
    return index;
}

fn returnStmt(p: *NodeParser) AnyErr!Ast.Index {
    if (p.check(.SEMICOLON)) return p.builder.nakedRet();
    const value = try p.expression();
    _ = try p.consume(.SEMICOLON, "Expected ';' after return");
    return p.builder.ret(value);
}

fn whileStmt(p: *NodeParser, comptime required_semicolon: bool) AnyErr!Ast.Index {
    _ = try p.consume(.LEFT_PAREN, "Expected '(' after while");
    const condition = try p.expression();
    _ = try p.consume(.RIGHT_PAREN, "Expected ')' after while condition");

    const body = try p.statement(required_semicolon);

    return p.builder.whileNode(condition, body);
}

fn ifStmt(p: *NodeParser, comptime required_semicolon: bool) AnyErr!Ast.Index {
    _ = try p.consume(.LEFT_PAREN, "Expected '(' after if");
    const condition = try p.expression();
    _ = try p.consume(.RIGHT_PAREN, "Expected ')' after if condition");

    const then_branch = try p.statement(required_semicolon);
    if (p.match(.ELSE)) |_| {
        const else_branch = try p.statement(required_semicolon);
        return p.builder.fullIf(condition, then_branch, else_branch);
    } else {
        return p.builder.simpleIf(condition, then_branch);
    }
}

fn block(p: *NodeParser) AnyErr!Ast.Index {
    var statements = std.ArrayList(Ast.Node).init(p.temp_node_allocator);
    defer statements.deinit();

    while (!p.check(.RIGHT_BRACE) and !p.isAtEnd()) {
        _ = try p.declaration(true);
        try statements.append(p.builder.node_list.pop());
    }

    _ = try p.consume(.RIGHT_BRACE, "Expected '}' after block");
    return p.builder.block(statements.items);
}

fn printStmt(p: *NodeParser, comptime required_semicolon: bool) AnyErr!Ast.Index {
    const value = try p.expression();
    if (required_semicolon) {
        _ = try p.consume(.SEMICOLON, "Expected ';' after expression");
    }
    return p.builder.print(value);
}

fn exprStmt(p: *NodeParser, comptime required_semicolon: bool) AnyErr!Ast.Index {
    const expr = try p.expression();
    if (required_semicolon) {
        _ = try p.consume(.SEMICOLON, "Expected ';' after expression");
    }
    return expr;
}

pub fn tryExpression(p: *NodeParser) AllocErr!?Ast.Index {
    return p.expression() catch |err| {
        if (err == error.ParseError) return null;
        return @errSetCast(AllocErr, err);
    };
}

inline fn expression(p: *NodeParser) AnyErr!Ast.Index {
    return p.assignment();
}

fn assignment(p: *NodeParser) AnyErr!Ast.Index {
    const expr = try p.logicOr();
    if (p.match(.EQUAL)) |equals| {
        const value = try p.logicOr();
        const slices = p.builder.node_list.slice();
        const new_node = switch (slices.items(.tag)[expr]) {
            .fetchVar => fetchVar: {
                const name = p.ast().unpack(Ast.FetchVar, expr);
                break :fetchVar NodeBuilder.assign(name, value);
            },
            .get => get: {
                const unpacked = p.ast().unpack(Ast.Get, expr);
                break :get NodeBuilder.set(
                    unpacked.name,
                    unpacked.obj,
                    value,
                );
            },
            else => {
                p.report(equals, "Invalid assignment target");
                return error.ParseError;
            },
        };

        // replace the node, since it's not a fetchVar/get anymore.
        p.builder.node_list.set(expr, new_node);
    }
    return expr;
}

fn logicOr(p: *NodeParser) AnyErr!Ast.Index {
    var expr = try p.logicAnd();
    while (p.match(.OR)) |op| {
        const right = try p.logicAnd();
        expr = try p.builder.binary(expr, right, op);
    }
    return expr;
}

fn logicAnd(p: *NodeParser) AnyErr!Ast.Index {
    var expr = try p.equality();
    while (p.match(.AND)) |op| {
        const right = try p.logicAnd();
        expr = try p.builder.binary(expr, right, op);
    }
    return expr;
}

fn equality(p: *NodeParser) AnyErr!Ast.Index {
    var expr = try p.comparison();
    while (p.matchMulti(&.{ .BANG_EQUAL, .EQUAL_EQUAL })) |op| {
        const right = try p.comparison();
        expr = try p.builder.binary(expr, right, op);
    }
    return expr;
}

fn comparison(p: *NodeParser) AnyErr!Ast.Index {
    var expr = try p.term();
    while (p.matchMulti(&.{ .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL })) |op| {
        const right = try p.term();
        expr = try p.builder.binary(expr, right, op);
    }
    return expr;
}

fn term(p: *NodeParser) AnyErr!Ast.Index {
    var expr = try p.factor();
    while (p.matchMulti(&.{ .MINUS, .PLUS })) |op| {
        const right = try p.factor();
        expr = try p.builder.binary(expr, right, op);
    }
    return expr;
}

// These cannot be easily folded, unless we use Pratt parsing.
fn factor(p: *NodeParser) AnyErr!Ast.Index {
    var expr = try p.unary();
    while (p.matchMulti(&.{ .SLASH, .STAR })) |op| {
        const right = try p.unary();
        expr = try p.builder.binary(expr, right, op);
    }
    return expr;
}

fn unary(p: *NodeParser) AnyErr!Ast.Index {
    // 1. Find first index that doesn't contain our unary operators
    const unary_ops = comptime Token.Mask.initMany(&.{ .BANG, .MINUS });
    const end_index = for (p.tokens[p.current..], p.current..) |t, i| {
        if (!unary_ops.contains(t.ty)) break i;
    } else p.tokens.len - 1; // use -1 for the EOF token. The rest of the parser relies on EOF not being skipped.

    const start_index = p.current;

    p.current = end_index;
    var expr = try p.call();

    // build the unary expression backwards.
    if (end_index != start_index) {
        try p.builder.node_list.ensureUnusedCapacity(
            p.builder.alloc,
            end_index - start_index,
        );
        var i: usize = end_index - 1;
        while (i >= start_index) : (i -= 1) {
            // can't run out of memory if we've already allocated it!
            expr = p.builder.unary(p.tokens[i], expr) catch unreachable;
        }
    }

    return expr;
}

fn call(p: *NodeParser) AnyErr!Ast.Index {
    var expr = try p.primary();
    while (true) {
        switch (p.advance().ty) {
            .LEFT_PAREN => {
                var args = std.ArrayList(Ast.Node).init(p.temp_node_allocator);
                defer args.deinit();
                if (!p.check(.RIGHT_PAREN)) {
                    // NOTE: I won't use 'swapRemove' because it's guaranteed
                    // to be the last pushed item. If it weren't, then I'd have
                    // to make sure that the nodes that depend on the last
                    // pushed node have their index pointing correctly. Knowing
                    // that it's a *future* thing, it's kinda hard.
                    _ = try p.expression();
                    try args.append(p.builder.node_list.pop());
                    while (p.match(.COMMA)) |_| {
                        if (args.items.len > 255) {
                            p.report(p.peek(), "Can't have more than 255 arguments");
                            return error.ParseError;
                        }
                        _ = try p.expression();
                        try args.append(p.builder.node_list.pop());
                    }
                }

                const paren = try p.consume(.RIGHT_PAREN, "Expected ')' after arguments");
                expr = try p.builder.call(paren, expr, args.items);
            },
            .DOT => {
                const name = try p.consume(.IDENTIFIER, "Expected property name after '.'");
                expr = try p.builder.get(name, expr);
            },
            else => {
                p.current -= 1;
                break;
            },
        }
    }
    return expr;
}

fn primary(p: *NodeParser) AnyErr!Ast.Index {
    switch (p.advance().ty) {
        .SUPER => {
            _ = try p.consume(.DOT, "Expected '.' after 'super'");
            const method = try p.consume(.IDENTIFIER, "Expected superclass method name");
            return p.builder.super(method);
        },
        .THIS => return p.builder.this(p.prev()),
        .FALSE, .TRUE, .NIL, .NUMBER, .STRING => return p.builder.literal(p.prev()),
        .FUN => return p.lambda(p.prev()),
        .IDENTIFIER => return p.builder.fetchVar(p.prev()),
        .LEFT_PAREN => {
            const inner = try p.expression();
            _ = try p.consume(.RIGHT_PAREN, "Expected ')' after expression");
            return inner;
        },
        else => {
            p.report(p.prev(), "Expected expression");
            return error.ParseError;
        },
    }
}

fn synchronize(p: *NodeParser) void {
    p.current += 1;
    while (!p.isAtEnd()) : (p.current += 1) {
        if (p.prev().ty == .SEMICOLON) break;

        const mask = comptime Token.Mask.initMany(&.{
            .CLASS, .FUN, .FOR, .IF, .WHILE, .PRINT, .RETURN,
        });

        if (mask.contains(p.peek().ty)) break;
    }
}

inline fn consume(
    p: *NodeParser,
    comptime ty: Token.Ty,
    comptime msg: []const u8,
) error{ParseError}!Token {
    if (!p.check(ty)) {
        p.report(p.peek(), msg);
        return error.ParseError;
    }
    return p.advance();
}

inline fn report(_: *const NodeParser, token: Token, comptime msg: []const u8) void {
    context.reportToken(token, msg);
}

inline fn matchMulti(p: *NodeParser, comptime types: []const Token.Ty) ?Token {
    const mask = comptime Token.Mask.initMany(types);
    const current = p.peek().ty;
    return if (mask.contains(current)) p.advance() else null;
}

inline fn match(p: *NodeParser, comptime ty: Token.Ty) ?Token {
    if (!p.check(ty)) return null;
    return p.advance();
}

inline fn check(p: *const NodeParser, comptime ty: Token.Ty) bool {
    return p.peek().ty == ty;
}

inline fn advance(p: *NodeParser) Token {
    if (!p.isAtEnd()) p.current += 1;
    return p.prev();
}

inline fn prev(p: *const NodeParser) Token {
    return p.tokens[p.current - 1];
}

inline fn isAtEnd(p: *const NodeParser) bool {
    return p.peek().ty == .EOF;
}

inline fn peek(p: *const NodeParser) Token {
    return p.tokens[p.current];
}

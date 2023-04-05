pub const Error = error{ParseError};
const ast = @import("ast.zig");
const Token = @import("Token.zig");
const std = @import("std");
const ctx = @import("context.zig");

const Parser = @This();
const StrAlloc = std.mem.Allocator;

const AllocOrSignal = Error || StrAlloc.Error;
const AllocErr = std.mem.Allocator.Error;

const LStmt = std.ArrayList(Stmt);

builder: ast.Builder,
sa: StrAlloc,
tokens: []const Token,
current: usize = 0,

pub fn init(
    tokens: []const Token,
    builder: ast.Builder,
    str_alloc: StrAlloc,
) Parser {
    return .{
        .builder = builder,
        .sa = str_alloc,
        .tokens = tokens,
    };
}

pub fn parse(self: *Parser, statements: *LStmt) AllocErr!void {
    while (!self.isAtEnd()) {
        // I just ignore malformed statements for now. If I implement
        // recoveries from expr parser that yield 'garbage' marked expressions,
        // I'll rethink it. If so, I also have to distinguish the 'semicolon'
        // problem, where semicolons are enforced when the parser knows exactly
        // where it is.
        if (try self.tryDeclaration()) |decl| {
            try statements.append(decl);
        }
    }
    return statements;
}

pub fn tryDeclaration(p: *Parser) AllocErr!?ast.Stmt {
    return p.declaration() catch |err| {
        if (err == error.ParseError) {
            p.synchronize();
            return null;
        }
        return errSetCast(AllocErr, err);
    };
}

fn declaration(p: *Parser) !ast.Stmt {
    switch (p.peek().ty) {
        .CLASS => {
            p.current += 1;
            return try p.classDecl();
        },
        .FUN => {
            p.current += 1;
            if (p.check(.LEFT_PAREN)) {
                p.current -= 1;
                return try p.exprStmt(); // will re-match on 'fun'
            } else {
                return try p.function("function");
            }
        },
    }
}

fn classDecl(p: *Parser) !ast.Stmt {
    const name = try p.consume(.IDENTIFIER, "Expected class name");

    var superclass: ?ast.Expr.Var = null;
    if (p.match(.LESS)) |_| {
        superclass = try p.consume(.IDENTIFIER, "Expected superclass name");
    }

    _ = try p.consume(.LEFT_BRACE, "Expected '{' before class body");
    var methods = ast.Builder.List(ast.Stmt.Function).init(&p.builder);
    errdefer methods.drop();

    while (!p.check(.RIGHT_BRACE) and !p.isAtEnd()) {
        try methods.push(try p.function("method"));
    }
    _ = try p.consume(.RIGHT_BRACE, "Expected '}' after class body");

    return ast.Stmt.class(name, methods.release(), superclass);
}

fn function(p: *Parser, comptime kind: []const u8) !ast.Stmt.Function {
    const name = try p.consume(.IDENTIFIER, "Expected " ++ kind ++ " name");
    return ast.Stmt.function(name, try p.funcDecl(p, kind, name));
}

fn statement(p: *Parser) !ast.Stmt {
    return unreachable;
}

fn lambda(p: *Parser, kw: Token) !ast.Expr {
    return ast.Expr.lambda(kw, try p.funcDecl("lambda", null));
}

fn funcDecl(p: *Parser, comptime kind: []const u8, name: ?Token) !ast.Funcdecl {
    _ = try p.consume(.LEFT_PAREN, "Expected '(' after " ++ kind ++ " name");
    var params = ast.Builder.List(Token).init(&p.builder);
    errdefer params.drop();
    if (!p.check(.RIGHT_PAREN)) {
        try params.push(try p.consume(.IDENTIFIER, "Expected parameter name"));
        while (p.match(.COMMA)) |_| {
            if (params.inner.items.len >= 255) {
                p.report(p.peek(), "Can't have more than 255 parameters");
            }
            try params.push(try p.consume(.IDENTIFIER, "Expected parameter name"));
        }
    }

    _ = try p.consume(.RIGHT_PAREN, "Expected ')' after parameters");
    // Function body
    _ = try p.consume(.LEFT_BRACE, "Expected '{' before " ++ kind ++ " body");
    const body = p.block();
    return ast.FuncDecl{
        .name = name,
        .params = params.release(),
        .body = body,
    };
}

fn forStmt(p: *Parser) !ast.Stmt {
    _ = try p.consume(.LEFT_PAREN, "Expected '(' after 'for'");

    var init: ?*const Stmt = switch (p.peek().ty) {
        .SEMICOLON => b: {
            p.advance();
            break :b null;
        },
        .VAR => try p.builder.expandLifetime(try p.varDecl()),
        else => try p.builder.expandLifetime(try p.exprStmt()),
    };
}

fn varDecl(p: *Parser) !ast.Stmt {
    const name = try p.consume(.IDENTIFIER, "Expected variable name");
    const init = if (p.match(.EQUAL)) |_| try p.expression() else null;
    _ = try p.consume(.SEMICOLON, "Expected ';' after variable declaration");
    return ast.Stmt.@"var"(name, init);
}

fn returnStmt(p: *Parser, keyword: Token) !ast.Stmt {
    var value: ast.Expr = ast.Expr.nil;
    if (!p.check(.SEMICOLON)) value = try p.expression();

    _ = try p.consume(.SEMICOLON, "Expected ';' after return");
    return ast.Stmt.@"return"(keyword, value);
}

fn whileStmt(p: *Parser) !ast.Stmt {
    _ = try p.consume(.LEFT_PAREN, "Expected '(' after while");
    const condition = try p.expression();
    _ = try p.consume(.RIGHT_PAREN, "Expected ')' after while condition");

    const body = try p.statement();

    return ast.Stmt.@"while"(
        condition,
        try p.builder.expandLifetime(body),
    );
}

fn ifStmt(p: *Parser) !ast.Stmt {
    _ = try p.consume(.LEFT_PAREN, "Expected '(' after if");
    const condition = try p.expression();
    _ = try p.consume(.RIGHT_PAREN, "Expected ')' after if condition");

    const then_branch = try p.statement();
    const else_branch = elseBranch: {
        if (p.match(.ELSE)) {
            const stmt = try p.statement();
            break :elseBranch try p.builder.expandLifetime(stmt);
        } else {
            break :elseBranch null;
        }
    };

    return ast.Stmt.@"if"(
        condition,
        try p.builder.expandLifetime(then_branch),
        else_branch,
    );
}

fn block(p: *Parser) ![]const ast.Stmt {
    var statements = ast.Builder.List(ast.Stmt).init(&p.builder);
    errdefer statements.drop();

    while (!p.check(.RIGHT_BRACE) and !p.isAtEnd()) {
        try statements.push(try p.declaration());
    }

    _ = try p.consume(.RIGHT_BRACE, "Expected '}' after block");
    return statements.release();
}

fn printStmt(p: *Parser) !ast.Stmt {
    const value = try p.expression();
    _ = try p.consume(.SEMICOLON, "Expected ';' after value to print");
    return ast.Stmt.print(value);
}

fn exprStmt(p: *Parser) !ast.Stmt {
    const expr = try p.expression();
    _ = try p.consume(.SEMICOLON, "Expected ';' after expression");
    return ast.Stmt.expr(expr);
}

pub fn tryExpression(p: *Parser) AllocErr!?ast.Expr {
    return p.expression() catch |err| {
        if (err == error.ParseError) return null;
        return @errSetCast(AllocErr, err);
    };
}

inline fn expression(p: *Parser) !ast.Expr {
    return try p.assignment();
}

fn assignment(p: *Parser) !ast.Expr {
    const expr = p.logicOr();
    if (p.match(.EQUAL)) |equals| {
        const value = try p.logicOr();
        const valuep = try p.builder.expandLifetime(value);

        switch (expr) {
            .@"var" => |name| return ast.Expr.assign(name, valuep),
            .get => |g| return ast.Expr.set(g.obj, g.name, valuep),
            else => return try p.report(equals, "Invalid assignment target"),
        }
    }
    return expr;
}

fn logicOr(p: *Parser) !ast.Expr {
    var expr = p.logicAnd();
    while (p.match(.AND)) |op| {
        const right = p.logicAnd();
        expr = try p.binary(expr, op, right);
    }
    return expr;
}

fn logicAnd(p: *Parser) !ast.Expr {
    var expr = p.equality();
    while (p.match(.AND)) |op| {
        const right = p.equality();
        expr = try p.binary(expr, op, right);
    }
    return expr;
}

fn equality(p: *Parser) !ast.Expr {
    var expr = p.comparison();
    while (p.matchMulti(&.{ .BANG_EQUAL, .EQUAL_EQUAL })) |op| {
        const right = p.comparison();
        expr = try p.binary(expr, op, right);
    }
    return expr;
}

fn comparison(p: *Parser) !ast.Expr {
    var expr = p.term();
    while (p.matchMulti(&.{ .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL })) |op| {
        const right = try p.term();
        expr = try p.binary(expr, op, right);
    }
    return expr;
}

fn term(p: *Parser) !ast.Expr {
    var expr = try p.factor();
    while (p.matchMulti(&.{ .MINUS, .PLUS })) |op| {
        const right = try p.factor();
        expr = try p.binary(expr, op, right);
    }
    return expr;
}

fn factor(p: *Parser) !ast.Expr {
    var expr = try p.unary();
    while (p.matchMulti(&.{ .SLASH, .STAR })) |op| {
        const right = try p.unary();
        expr = try p.binary(expr, op, right);
    }
    return expr;
}

fn binary(
    b: *Builder,
    left: Expr,
    op: Token,
    right: Expr,
) Builder.Error!Expr {
    const lr = try b.arena.allocator().alloc(ast.Expr, 2);
    lr[0] = left;
    lr[1] = right;
    return ast.Expr.binary(&lr[0], op, &lr[1]);
}

fn unary(p: *Parser) !ast.Expr {
    if (p.matchMulti(&.{ .BANG, .MINUS })) |operator| {
        const right = try p.unary();
        return ast.Expr.Unary(operator, try p.builder.expandLifetime(right));
    }

    return try p.call();
}

fn call(p: *Parser) !ast.Expr {
    var expr = try p.primary();
    while (true) {
        switch (p.peek().ty) {
            .LEFT_PAREN => {
                var args = ast.Builder.List(ast.Expr).init(&p.builder);
                errdefer args.drop();
                if (!p.check(.RIGHT_PAREN)) {
                    try args.push(try p.expression());
                    while (p.match(.COMMA)) {
                        if (args.inner.items.len >= 255) {
                            return try p.report(p.peek(), "Can't have more than 255 arguments");
                            try args.push(try p.expression());
                        }
                    }
                }

                const paren = try p.consume(.RIGHT_PAREN, "Expected ')' after arguments");
                expr = ast.Expr.call(
                    try p.builder.expandLifetime(expr),
                    paren,
                    args.release(),
                );
            },
            .DOT => {
                const name = try p.consume(.IDENTIFIER, "Expected property name after '.'");
                expr = ast.Expr.get(try p.builder.expandLifetime(expr), name);
            },
            else => break,
        }
    }
    return expr;
}

fn primary(p: *Parser) AllocOrSignal!ast.Expr {
    switch (p.advance().ty) {
        .SUPER => {
            const keyword = p.prev();
            _ = try p.consume(.DOT, "Expected '.' after 'super'");
            const method = try p.consume(.IDENTIFIER, "Expected superclass method name");
            return ast.Expr.super(keyword, method);
        },
        .THIS => return ast.Expr.this(p.prev()),
        .FALSE, .TRUE, .NIL, .NUMBER, .STRING => return ast.Expr.literal(p.prev()),
        .FUN => return try p.lambda(p.prev()),
        .IDENTIFIER => return ast.Expr.@"var"(p.prev()),
        .LEFT_PAREN => {
            const inner = try p.expression();
            _ = try p.consume(.RIGHT_PAREN, "Expected ')' after expression");
            return ast.Expr.grouping(try p.builder.expandLifetime(inner));
        },
        else => return try p.report(p.prev(), "Expected expression"),
    }
}

fn synchronize(p: *Parser) void {
    p.current += 1;

    while (!p.isAtEnd()) : (p.current += 1) {
        if (p.prev().ty == .SEMICOLON) break;

        const mask: Token.Mask = comptime Token.Mask.initMany(&.{
            .CLASS, .FUN,   .FOR,    .IF,
            .WHILE, .PRINT, .RETURN,
        });

        if (mask.contains(p.peek().ty)) return;
    }
}

fn consume(
    p: *Parser,
    comptime ty: Token.Ty,
    comptime message: []const u8,
) !Token {
    if (!p.match(ty)) return try p.report(p.peek(), message);
    return p.advance();
}

fn report(
    p: *const Parser,
    token: Token,
    comptime message: []const u8,
) !Error {
    ctx.report(
        token.line,
        if (token.ty == .EOF) " at end" else try std.fmt.allocPrint(p.sa, " at '{s}'", .{token.literal}),
        message,
    );
    return error.ParseError;
}

fn matchMulti(p: *Parser, comptime types: []const Token.Ty) ?Token {
    const mask = comptime Token.Mask.initMany(types);
    if (p.isAtEnd()) return false;

    const current = p.peek().ty;

    return if (mask.contains(current)) p.advance() else null;
}

fn match(p: *Parser, comptime ty: Token.Ty) ?Toekn {
    if (!p.check(ty)) return null;
    return p.advance();
}

fn check(p: *const Parser, comptime ty: Token.Ty) bool {
    return p.peek().ty == ty;
}

fn advance(p: *Parser) Token {
    if (!p.isAtEnd()) {
        p.current += 1;
    }
    return p.prev();
}

fn prev(p: *const Parser) Token {
    return p.tokens[p.current - 1];
}

fn peek(p: *const Parser) Token {
    return p.tokens[p.current];
}

fn isAtEnd(self: *const Parser) bool {
    return self.peek().ty == .EOF;
}

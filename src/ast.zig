//! All the definitions for a naive readonly AST structure.
//! Basic optimization has been done to remove building components:
//!     - no null pointers,
//!     - slices instead of vecs
//!     - no slices of pointers
//!     - unified calling of visitor methods through tags
//!     - no storage of allocators (will be all managed through an arena)
//! Other remain:
//!     - using pointers to refer to children (misses)

const std = @import("std");
const Token = @import("Token.zig");
pub const Builder = @import("ast/Builder.zig");
pub const Printer = @import("ast/Printer.zig");

pub const Expr = union(enum(u4)) {
    binary: Binary,
    // the book wants to go with the AST.
    literal: Literal,
    unary: Unary,
    @"var": Expr.Var,
    lambda: Lambda,
    assign: Assign,
    call: Call,
    this: Token,
    super: Super,
    get: Get,
    set: Set,

    pub const Literal = Token.TaggedLiteral;

    // count == 11 => u4

    pub fn @"var"(name: Token) Expr {
        return .{ .@"var" = name };
    }

    pub const nil: Expr = .{ .literal = .{ .nil = {} } };

    pub fn literal(token: Token) Expr {
        return .{ .literal = token.extractLiteral() };
    }

    pub fn binary(
        left: *const Expr,
        op: Token,
        right: *const Expr,
    ) Expr {
        return .{ .binary = Binary{
            .left = left,
            .operator = op,
            .right = right,
        } };
    }

    pub fn unary(operator: Token, right: *const Expr) Expr {
        return .{ .unary = Expr.Unary{ .operator = operator, .right = right } };
    }

    pub fn grouping(e: *const Expr) Expr {
        return .{ .grouping = e };
    }

    pub fn lambda(keyword: Token, decl: FuncDecl) Expr {
        return .{ .lambda = Expr.Lambda{ .keyword = keyword, .decl = decl } };
    }

    pub fn assign(name: Token, value: *const Expr) Expr {
        return .{ .assign = Expr.Assign{ .name = name, .value = value } };
    }

    pub fn call(
        callee: *const Expr,
        paren: Token,
        arguments: []const Expr,
    ) Expr {
        return .{ .call = Expr.Call{
            .callee = callee,
            .paren = paren,
            .arguments = arguments,
        } };
    }

    pub fn this(keyword: Token) Expr {
        return .{ .this = keyword };
    }

    pub fn super(keyword: Token, method: Token) Expr {
        return .{ .super = Expr.Super{ .keyword = keyword, .method = method } };
    }

    pub fn get(object: *const Expr, name: Token) Expr {
        return .{ .get = Expr.Get{ .obj = object, .name = name } };
    }

    pub fn set(
        object: *const Expr,
        value: *const Expr,
        name: Token,
    ) Expr {
        return .{ .set = Expr.Set{
            .obj = object,
            .value = value,
            .name = name,
        } };
    }

    pub const Var = Token;
    pub const Set = struct {
        obj: *const Expr,
        value: *const Expr,
        name: Token,
    };

    pub const Get = struct {
        obj: *const Expr,
        name: Token,
    };

    pub const Super = struct {
        keyword: Token,
        method: Token,
    };

    pub const Call = struct {
        paren: Token,
        callee: *const Expr,
        arguments: []const Expr,
    };

    pub const Assign = struct {
        name: Token,
        value: *const Expr,
    };

    pub const Lambda = struct {
        keyword: Token,
        decl: FuncDecl,
    };

    pub const Binary = struct {
        left: *const Expr,
        operator: Token,
        right: *const Expr,
    };

    pub const Unary = struct {
        operator: Token,
        right: *const Expr,
    };
};

pub const Stmt = union(enum(u4)) {
    expr: Expr,
    function: Function,
    @"if": If,
    @"return": Return,
    print: Print,
    @"var": Var,
    @"while": While,
    block: []const Stmt,
    class: Class,

    // count == 9 => u4

    pub const Class = struct {
        name: Token,
        methods: []const Function,
        superclass: ?Expr.Var,
    };

    pub const While = struct {
        condition: Expr,
        body: *const Stmt,
    };

    pub const Var = struct {
        name: Token,
        init: ?Expr,
    };

    pub const Return = struct {
        keyword: Token,
        value: Expr,
    };

    pub const Print = Expr;

    pub const If = struct {
        condition: Expr,
        then_branch: *const Stmt,
        else_branch: ?*const Stmt,
    };

    pub const Function = struct {
        name: Token,
        decl: FuncDecl,
    };

    pub fn expr(e: Expr) Stmt {
        return .{ .expr = e };
    }

    pub fn function(name: Token, decl: FuncDecl) Stmt {
        return .{ .function = Function{ .name = name, .decl = decl } };
    }

    pub fn @"if"(
        condition: Expr,
        then_branch: *const Stmt,
        else_branch: ?*const Stmt,
    ) Stmt {
        return .{ .@"if" = If{
            .condition = condition,
            .then_branch = then_branch,
            .else_branch = else_branch,
        } };
    }

    pub fn @"return"(keyword: Token, value: Expr) Stmt {
        return .{ .@"return" = Return{ .keyword = keyword, .value = value } };
    }

    pub fn print(e: Expr) Stmt {
        return .{ .print = e };
    }

    pub fn @"var"(name: Token, init: ?Expr) Stmt {
        return .{ .@"var" = Var{ .name = name, .init = init } };
    }

    pub fn @"while"(condition: Expr, body: *const Stmt) Stmt {
        return .{ .@"while" = While{ .condition = condition, .body = body } };
    }

    pub fn block(stmts: []const Stmt) Stmt {
        return .{ .block = stmts };
    }

    pub fn class(
        name: Token,
        methods: []const Stmt.Function,
        superclass: ?Expr.Var,
    ) Stmt {
        return .{ .class = .{
            .name = name,
            .methods = methods,
            .superclass = superclass,
        } };
    }
};
pub const FuncDecl = struct {
    name: ?Token,
    params: []const Token,
    body: []const Stmt,
};

pub fn printExpr(e: Expr) void {
    var printer = Printer{};
    printer.printExpr(e);
}

pub fn printStmt(e: Stmt) void {
    var printer = Printer{};
    printer.printStmt(e);
}

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
    grouping: *const Expr, // this is not really needed, but I don't know where
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

    // count == 12 => u4

    pub fn VisitorVTable(comptime R: type, comptime Itor: type) type {
        return struct {
            visitBinary: fn (*Itor, Binary) R,
            visitLiteral: fn (*Itor, Literal) R,
            visitUnary: fn (*Itor, Unary) R,
            visitVar: fn (*Itor, Token) R,
            visitLambda: fn (*Itor, Lambda) R,
            visitAssign: fn (*Itor, Assign) R,
            visitCall: fn (*Itor, Call) R,
            visitThis: fn (*Itor, Token) R,
            visitSuper: fn (*Itor, Super) R,
            visitGet: fn (*Itor, Get) R,
            visitSet: fn (*Itor, Set) R,
        };
    }

    pub fn accept(
        self: Expr,
        comptime R: type,
        comptime Itor: type,
        comptime vt: VisitorVTable(R, Itor),
        v: *Itor,
    ) R {
        return switch (self) {
            .binary => |bin| vt.visitBinary(v, bin),
            .grouping => |g| g.accept(R, Itor, vt, v),
            .literal => |l| vt.visitLiteral(v, l),
            .unary => |u| vt.visitUnary(v, u),
            .@"var" => |t| vt.visitVar(v, t),
            .lambda => |l| vt.visitLambda(v, l),
            .assign => |a| vt.visitAssign(v, a),
            .call => |c| vt.visitCall(v, c),
            .this => |t| vt.visitThis(v, t),
            .super => |s| vt.visitSuper(v, s),
            .get => |g| vt.visitGet(v, g),
            .set => |s| vt.visitSet(v, s),
        };
    }

    pub fn @"var"(name: Token) Expr {
        return .{ .@"var" = name };
    }

    pub const nil = .{ .literal = .{ .nil = {} } };

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

    pub fn VisitorVTable(comptime R: type, comptime I: type) type {
        return struct {
            visitExpr: fn (*I, Expr) R,
            visitFunction: fn (*I, Function) R,
            visitIf: fn (*I, If) R,
            visitReturn: fn (*I, Return) R,
            visitPrint: fn (*I, Expr) R,
            visitVar: fn (*I, Var) R,
            visitWhile: fn (*I, While) R,
            visitBlock: fn (*I, []const Stmt) R,
            visitClass: fn (*I, Class) R,
        };
    }

    pub fn accept(
        self: Stmt,
        comptime R: type,
        comptime I: type,
        comptime vt: VisitorVTable(R, I),
        v: *I,
    ) R {
        return switch (self) {
            .expr => |e| vt.visitExpr(v, e),
            .function => |f| vt.visitFunction(v, f),
            .@"if" => |i| vt.visitIf(v, i),
            .@"return" => |r| vt.visitReturn(v, r),
            .print => |p| vt.visitPrint(v, p),
            .@"var" => |va| vt.visitVar(v, va),
            .@"while" => |wh| vt.visitWhile(v, wh),
            .block => |bl| vt.visitBlock(v, bl),
            .class => |cl| vt.visitClass(v, cl),
        };
    }
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
    name: ?Token, // NOTE: this is not needed, since it's included in Function.
    params: []const Token,
    body: []const Stmt,
};

const builtin = std.builtin;
pub const Error = std.mem.Allocator.Error;
const NodeBuilder = @This();
const Token = @import("../Token.zig");
const Ast = @import("Ast.zig");
const std = @import("std");
const interpreter = @import("../interpreter/data.zig");

extra_data: std.ArrayListUnmanaged(Ast.Index) = .{},
annotated_tokens: std.ArrayListUnmanaged(Token) = .{},
literals: std.ArrayListUnmanaged(interpreter.Value) = .{},
node_list: Ast.NodeList = .{},
alloc: std.mem.Allocator,

// TODO: consider reversing the node array at the end,
// having to patch the indices when accessing them (i.e index -> final len - index - 1)

pub inline fn ast(b: *const NodeBuilder) Ast {
    return Ast{
        .nodes = b.node_list.slice(),
        .extra_data = b.extra_data.items,
        .literals = b.literals.items,
        .tokens = b.annotated_tokens.items,
    };
}

pub fn deinit(b: *NodeBuilder) void {
    b.extra_data.deinit(b.alloc);
    b.node_list.deinit(b.alloc);
    b.annotated_tokens.deinit(b.alloc);
}

pub inline fn annotateToken(b: *NodeBuilder, tok: Token) Error!void {
    return b.annotated_tokens.append(b.alloc, tok);
}

pub inline fn dropTokenAnnotate(b: *NodeBuilder, stamp: Ast.Index) void {
    b.annotated_tokens.items.len = stamp;
}

pub inline fn stampTokenAnnotate(b: *const NodeBuilder) Ast.Index {
    return @truncate(Ast.Index, b.annotated_tokens.items.len);
}

pub fn class(
    b: *NodeBuilder,
    name: Token,
    superclass: Token,
    methods: []const Ast.Node,
) Error!Ast.Index {
    const superclass_tk_index = @intCast(Ast.Index, b.annotated_tokens.items.len);
    try b.annotateToken(superclass);
    try b.node_list.ensureUnusedCapacity(b.alloc, methods.len + 1);
    const class_data = Ast.Node.Class{
        .superclass = superclass_tk_index,
        .methods = b.nodesAssumeCapacity(methods),
    };
    const class_index = try b.writeExtraData(Ast.Node.Class, class_data);
    return b.nodeAssumeCapacity(.{
        .tag = .class,
        .token = name,
        .data = .{ .lhs = undefined, .rhs = class_index },
    });
}

pub inline fn singleClass(
    b: *NodeBuilder,
    name: Token,
    methods: []const Ast.Node,
) Error!Ast.Index {
    try b.node_list.ensureUnusedCapacity(b.alloc, methods.len + 1);
    return b.nodeAssumeCapacity(.{
        .tag = .single_class,
        .token = name,
        .data = Ast.Node.Data.fromRange(b.nodesAssumeCapacity(methods)),
    });
}

pub inline fn block(
    b: *NodeBuilder,
    inner: []const Ast.Node,
) Error!Ast.Index {
    try b.node_list.ensureUnusedCapacity(b.alloc, inner.len + 1);
    return b.nodeAssumeCapacity(.{
        .tag = .block,
        .token = undefined,
        .data = Ast.Node.Data.fromRange(b.nodesAssumeCapacity(inner)),
    });
}

pub inline fn nakedWhile(b: *NodeBuilder, body: Ast.Index) Error!Ast.Index {
    return b.node(.{
        .tag = .naked_while,
        .token = undefined,
        .data = .{ .rhs = body, .lhs = undefined },
    });
}

pub inline fn whileNode(
    b: *NodeBuilder,
    cond: Ast.Index,
    body: Ast.Index,
) Error!Ast.Index {
    return b.node(.{
        .tag = .@"while",
        .token = undefined,
        .data = .{ .lhs = cond, .rhs = body },
    });
}

pub inline fn simpleIf(
    b: *NodeBuilder,
    cond: Ast.Index,
    then: Ast.Index,
) Error!Ast.Index {
    return b.node(.{
        .tag = .if_simple,
        .token = undefined,
        .data = .{ .lhs = cond, .rhs = then },
    });
}

pub inline fn fullIf(
    b: *NodeBuilder,
    cond: Ast.Index,
    then_branch: Ast.Index,
    else_branch: Ast.Index,
) Error!Ast.Index {
    const data = Ast.Node.If{ .then_index = then_branch, .else_index = else_branch };
    const data_index = try b.writeExtraData(Ast.Node.If, data);
    return b.node(.{
        .tag = .@"if",
        .token = undefined,
        .data = .{ .rhs = data_index, .lhs = cond },
    });
}

pub inline fn function(
    b: *NodeBuilder,
    name: Token,
    decl: Ast.Node.FuncDecl,
) Error!Ast.Index {
    const decl_index = try b.writeExtraData(Ast.Node.FuncDecl, decl);
    return b.node(.{
        .tag = .function,
        .token = name,
        .data = .{ .rhs = decl_index, .lhs = undefined },
    });
}

pub inline fn initVarDecl(
    b: *NodeBuilder,
    name: Token,
    init: Ast.Index,
) Error!Ast.Index {
    return b.node(.{
        .tag = .init_var_decl,
        .token = name,
        .data = .{ .rhs = init, .lhs = undefined },
    });
}

pub inline fn nakedVarDecl(b: *NodeBuilder, name: Token) Error!Ast.Index {
    return b.node(.{
        .tag = .naked_var_decl,
        .token = name,
        .data = undefined,
    });
}

pub inline fn nakedRet(b: *NodeBuilder) Error!Ast.Index {
    return b.node(.{
        .tag = .naked_ret,
        .token = undefined,
        .data = undefined,
    });
}

pub inline fn ret(b: *NodeBuilder, value: Ast.Index) Error!Ast.Index {
    return b.node(.{
        .tag = .ret,
        .token = undefined,
        .data = .{ .rhs = value, .lhs = undefined },
    });
}

pub inline fn print(b: *NodeBuilder, value: Ast.Index) Error!Ast.Index {
    return b.node(.{
        .tag = .print,
        .token = undefined, // nothing can error here ;)
        // NOTE: could be used for debug info though.
        .data = .{ .rhs = value, .lhs = undefined },
    });
}

pub inline fn set(
    name: Token,
    obj: Ast.Index,
    value: Ast.Index,
) Ast.Node {
    return .{
        .tag = .set,
        .token = name,
        .data = .{ .lhs = obj, .rhs = value },
    };
}

pub inline fn get(
    b: *NodeBuilder,
    name: Token,
    rhs: Ast.Index,
) Error!Ast.Index {
    return b.node(.{
        .tag = .get,
        .token = name,
        .data = .{ .rhs = rhs, .lhs = undefined },
    });
}

pub inline fn super(b: *NodeBuilder, method: Token) Error!Ast.Index {
    return b.node(.{
        .tag = .super,
        .token = method,
        .data = undefined,
    });
}

pub inline fn this(b: *NodeBuilder, token: Token) Error!Ast.Index {
    return b.node(.{
        .tag = .this,
        .token = token,
        .data = undefined,
    });
}

pub inline fn call(
    b: *NodeBuilder,
    paren: Token,
    callee: Ast.Index,
    params: []const Ast.Node,
) Error!Ast.Index {
    try b.node_list.ensureUnusedCapacity(b.alloc, params.len + 1);
    const params_slice = b.nodesAssumeCapacity(params);
    const params_index = try b.writeExtraData(Ast.SliceIndex, params_slice);
    return b.node(.{
        .tag = .call,
        .token = paren,
        .data = .{ .lhs = callee, .rhs = params_index },
    });
}

pub inline fn assign(
    name: Token,
    value: Ast.Index,
) Ast.Node {
    return .{
        .tag = .assign,
        .token = name,
        .data = .{ .lhs = undefined, .rhs = value },
    };
}

pub inline fn fetchVar(b: *NodeBuilder, name: Token) Error!Ast.Index {
    return b.node(.{
        .tag = .fetchVar,
        .token = name,
        .data = undefined,
    });
}

pub inline fn binary(
    b: *NodeBuilder,
    left: Ast.Index,
    right: Ast.Index,
    op: Token,
) Error!Ast.Index {
    return b.node(.{
        .tag = .binary,
        .token = op,
        .data = .{ .lhs = left, .rhs = right },
    });
}

pub inline fn literal(b: *NodeBuilder, lit_token: Token) Error!Ast.Index {
    const literal_index = @intCast(Ast.Index, b.literals.items.len);
    try b.literals.append(
        b.alloc,
        interpreter.literalToValue(lit_token),
    );
    return b.node(.{
        .tag = .literal,
        .token = lit_token,
        .data = .{ .lhs = literal_index, .rhs = undefined },
    });
}

pub inline fn unary(b: *NodeBuilder, op: Token, rhs: Ast.Index) Error!Ast.Index {
    return b.node(.{
        .tag = .unary,
        .token = op,
        .data = .{ .lhs = undefined, .rhs = rhs },
    });
}

pub inline fn node(b: *NodeBuilder, n: Ast.Node) Error!Ast.Index {
    const index = b.node_list.len;
    try b.node_list.append(b.alloc, n);
    return @truncate(Ast.Index, index);
}

inline fn nodeAssumeCapacity(b: *NodeBuilder, n: Ast.Node) Ast.Index {
    const index = b.node_list.len;
    b.node_list.appendAssumeCapacity(n);
    return @truncate(Ast.Index, index);
}

fn writeExtraData(
    b: *NodeBuilder,
    comptime T: type,
    value: T,
) Error!Ast.Index {
    const start_index = b.extra_data.items.len;
    const fields = std.meta.fields(T);
    const index_count = comptime indexCount(fields);

    try b.extra_data.ensureUnusedCapacity(b.alloc, index_count);

    inline for (fields) |f| {
        switch (f.type) {
            Ast.SliceIndex => {
                const ind: Ast.SliceIndex = @field(value, f.name);
                b.extra_data.appendAssumeCapacity(ind.start);
                b.extra_data.appendAssumeCapacity(ind.end);
            },
            Ast.Index => {
                const index: Ast.Index = @field(value, f.name);
                b.extra_data.appendAssumeCapacity(index);
            },
            else => unreachable,
        }
    }

    return @truncate(Ast.Index, start_index);
}

fn indexCount(comptime fields: []const builtin.Type.StructField) usize {
    var sum: usize = 0;
    inline for (fields) |f| {
        sum += switch (f.type) {
            Ast.SliceIndex => 2,
            Ast.Index => 1,
            else => @compileError("field " ++ f.name ++ ": must be either SliceIndex or Index, got " ++
                @typeName(f.type) ++ " instead"),
        };
    }
    return sum;
}

inline fn nodesAssumeCapacity(b: *NodeBuilder, nodes: []const Ast.Node) Ast.SliceIndex {
    const slice: Ast.SliceIndex = .{
        .start = @intCast(Ast.Index, b.node_list.len),
        .end = @intCast(Ast.Index, b.node_list.len + nodes.len),
    };

    for (nodes) |n| b.node_list.appendAssumeCapacity(n);

    return slice;
}

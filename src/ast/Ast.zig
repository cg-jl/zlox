const Ast = @This();
const std = @import("std");
const Token = @import("../Token.zig");
const interpreter = @import("../interpreter/data.zig");

nodes: NodeList.Slice,
extra_data: []const Index,
tokens: []const Token,
literals: []const interpreter.Value,

pub const NodeList = std.MultiArrayList(Node);

pub const Index = u32;
pub const SliceIndex = struct {
    start: Index,
    end: Index,

    pub inline fn len(ind: SliceIndex) u32 {
        return ind.end - ind.start;
    }
};

pub fn extraData(tree: Ast, comptime T: type, index: usize) T {
    var result: T = undefined;
    const fields = comptime std.meta.fields(T);
    var i: usize = 0;
    inline for (fields) |f| {
        const field: std.builtin.Type.StructField = f;
        if (field.type == SliceIndex) {
            @field(result, field.name) = SliceIndex{
                .start = tree.extra_data[index + i],
                .end = tree.extra_data[index + i + 1],
            };
            i += 2;
        } else {
            comptime std.debug.assert(field.type == Index);
            @field(result, field.name) = tree.extra_data[index + i];
            i += 1;
        }
    }
    return result;
}

pub const Node = struct {
    tag: Tag,
    source: Token.Source,
    token_type: Token.Ty,
    data: Data,

    pub const Data = struct {
        lhs: Index,
        rhs: Index,

        pub inline fn fromRange(s: SliceIndex) Data {
            return .{ .lhs = s.start, .rhs = s.end };
        }
        pub inline fn toRange(d: Data) SliceIndex {
            return .{ .start = d.lhs, .end = d.rhs };
        }
    };

    // NOTE: if changing the layout of any of these, make sure to change
    // `unpack` and NodeBuilder accordingly!!!

    // count == 9 + 10 = 19 => u5
    pub const Tag = enum(u5) {
        binary, // lhs, rhs. Token == operator.
        literal, // token == literal (for debug purposes? location?), lhs ==
        // literal index.
        unary, // rhs, token == operator.
        fetchVar, // token.
        assign, // token == name, rhs == value
        call, // token == paren, lhs == callee, rhs == (extra) SliceIndex params
        this, // token == this.
        super, // token == this. rhs == method (token index)
        get, // token == name. rhs == expr index
        set, // token == name. lhs == obj, rhs == value
        print, // token == name. rhs == value.
        ret, // token == undefined, rhs == value.
        naked_ret, // no data. (token == undefined, data == undefined)
        naked_var_decl, // token == name.
        init_var_decl, // token == name, rhs == init
        // expr is implicit.
        function, // token == name. rhs == (extra) decl index
        lambda, // same as function.
        @"if", // token == name, lhs == cond, rhs == (extra) if index
        if_simple, // token == undefined, lhs == cond, rhs == then_index
        naked_while, // token == undefined, rhs == body_index.
        @"while", // token == undefined, lhs == cond, rhs == body_index
        block, // token == undefined, lhs == start, rhs == end.
        single_class, // token == name, lhs = methods start, rhs = methods end
        // TODO: check if name is really needed.
        class, // token == name,  rhs = (extra) Class
    };

    pub const Class = struct {
        superclass: Index, // note: this indexes the token array, not the nodes.
        methods: SliceIndex,
    };

    pub const If = struct {
        then_index: Index,
        else_index: Index,
    };

    pub const FuncDecl = struct {
        params: SliceIndex, // note: this slices the tokens array, not the nodes.
        body: SliceIndex,
    };

    pub const CallData = struct {
        callee: Index,
        params: SliceIndex,
    };
};

pub inline fn unpack(
    tree: Ast,
    comptime T: type,
    index: usize,
) T {
    switch (T) {
        Literal => {
            const lit_index = tree.nodes.items(.data)[index].lhs;
            return tree.literals[lit_index];
        },
        Binary => {
            const data: Node.Data = tree.nodes.items(.data)[index];
            const op: Token.Ty = tree.nodes.items(.token_type)[index];
            return Binary{
                .lhs = data.lhs,
                .rhs = data.rhs,
                .op = op,
            };
        },
        Node.Data => return tree.nodes.items(.data)[index],
        Token.Ty => return tree.nodes.items(.token_type)[index],
        Token.Source => return tree.nodes.items(.source)[index],
        Unary => {
            const data: Node.Data = tree.nodes.items(.data)[index];
            const op: Token.Ty = tree.nodes.items(.token_type)[index];
            return Unary{ .op = op, .rhs = data.rhs };
        },
        Assign => {
            const name = tree.nodes.items(.source)[index];
            const data: Node.Data = tree.nodes.items(.data)[index];
            return Assign{ .name = name, .rhs = data.rhs };
        },
        Call => {
            const data: Node.Data = tree.nodes.items(.data)[index];
            const params = tree.extraData(SliceIndex, data.rhs);
            return Call{
                .callee = data.lhs,
                .params = params,
            };
        },
        Get => {
            const name = tree.nodes.items(.source)[index];
            const data: Node.Data = tree.nodes.items(.data)[index];
            return Get{ .name = name, .obj = data.rhs };
        },
        Set => {
            const name = tree.nodes.items(.source)[index];
            const data: Node.Data = tree.nodes.items(.data)[index];
            return Set{
                .name = name,
                .obj = data.lhs,
                .value = data.rhs,
            };
        },
        Ast.Node.FuncDecl => {
            const data: Node.Data = tree.nodes.items(.data)[index];
            return tree.extraData(Ast.Node.FuncDecl, data.rhs);
        },
        Function => {
            const name = tree.nodes.items(.source)[index];
            const data: Node.Data = tree.nodes.items(.data)[index];
            const func_decl = tree.extraData(Ast.Node.FuncDecl, data.rhs);
            return Function{ .name = name, .decl = func_decl };
        },

        FullClass => {
            const name = tree.nodes.items(.source)[index];
            const data: Node.Data = tree.nodes.items(.data)[index];
            const class: Node.Class = tree.extraData(Node.Class, data.rhs);
            return FullClass{
                .name = name,
                .superclass = tree.tokens[class.superclass],
                .methods = class.methods,
            };
        },

        SingleClass => {
            const name = tree.nodes.items(.source)[index];
            const data: Node.Data = tree.nodes.items(.data)[index];
            return SingleClass{ .name = name, .methods = data.toRange() };
        },

        Rhs => {
            const data: Node.Data = tree.nodes.items(.data)[index];
            return data.rhs;
        },

        InitVarDecl => {
            const name = tree.nodes.items(.source)[index];
            const data: Node.Data = tree.nodes.items(.data)[index];
            return InitVarDecl{ .name = name, .init = data.rhs };
        },

        IfSimple => {
            const data: Node.Data = tree.nodes.items(.data)[index];
            return IfSimple{ .cond = data.lhs, .then_branch = data.rhs };
        },

        FullIf => {
            const data: Node.Data = tree.nodes.items(.data)[index];
            const extra: Node.If = tree.extraData(Node.If, data.rhs);
            return FullIf{
                .cond = data.lhs,
                .then_branch = extra.then_index,
                .else_branch = extra.else_index,
            };
        },

        While => {
            const data: Node.Data = tree.nodes.items(.data)[index];
            return While{ .cond = data.lhs, .body = data.rhs };
        },

        Block => {
            const data: Node.Data = tree.nodes.items(.data)[index];
            return data.toRange();
        },

        IndexedToken => return IndexedToken{ .tok = tree.tokens[index] },

        else => @compileError(@typeName(T) ++ " not registered how to unpack"),
    }
}

pub const IndexedToken = struct { tok: Token };

pub const Block = SliceIndex;

pub const While = struct {
    cond: Index,
    body: Index,
};

pub const NakedWhile = Index;

pub const FullIf = struct {
    cond: Index,
    then_branch: Index,
    else_branch: Index,
};

pub const IfSimple = struct {
    cond: Index,
    then_branch: Index,
};

pub const InitVarDecl = struct {
    name: Token.Source,
    init: Index,
};

pub const NakedVarDecl = Token.Source;

pub const Return = Rhs;

pub const Print = Rhs;

pub const Rhs = Index;

pub const SingleClass = struct {
    name: Token.Source,
    methods: SliceIndex,
};

pub const FullClass = struct {
    name: Token.Source,
    superclass: Token,
    methods: SliceIndex,
};

pub const Function = struct {
    name: Token.Source,
    decl: Node.FuncDecl,
};

pub const Set = struct {
    name: Token.Source,
    obj: Index,
    value: Index,
};

pub const Super = Token.Source;

pub const This = Token.Source;

pub const Get = struct {
    name: Token.Source,
    obj: Index,
};

pub const Call = struct {
    callee: Index,
    params: SliceIndex,
};

pub const Binary = struct {
    lhs: Index,
    rhs: Index,
    op: Token.Ty,
};

pub const Literal = interpreter.Value;

pub const Unary = struct {
    op: Token.Ty,
    rhs: Index,
};

pub const FetchVar = Token.Source;

pub const Assign = struct {
    name: Token.Source,
    rhs: Index,
};

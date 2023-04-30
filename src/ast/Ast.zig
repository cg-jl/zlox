const Ast = @This();
const std = @import("std");
const Token = @import("../Token.zig");

extra_data: []const Index,
tokens: []const Token,
nodes: NodeList.Slice,

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
    const fields = std.meta.fields(T);
    var i: usize = 0;
    inline for (fields) |f| {
        const field: std.builtin.Type.StructField = f;
        if (field.type == SliceIndex) {
            @field(result, field.name) = tree.extraData(index + i, SliceIndex);
            i += 2;
        } else {
            comptime std.debug.assert(field.type == Index);
            @field(result, field.name) = tree.extra_data[index + i];
            i += 1;
        }
    }
}

pub const Node = struct {
    tag: Tag,
    token: Token,
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
        literal, // token.
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
    index: Index,
) T {
    switch (T) {
        Binary => {
            const data: Node.Data = tree.nodes.items(.data)[index];
            const op: Token = tree.nodes.items(.token)[index];
            return Binary{
                .lhs = data.lhs,
                .rhs = data.rhs,
                .op = op,
            };
        },
        Token => return tree.nodes.items(.token)[index],
        Unary => {
            const data: Node.Data = tree.nodes.items(.data)[index];
            const op: Token = tree.nodes.items(.token)[index];
            return Unary{ .op = op, .rhs = data.rhs };
        },
        Assign => {
            const name = tree.nodes.items(.token)[index];
            const data: Node.Data = tree.nodes.items(.data)[index];
            return Assign{ .name = name, .rhs = data.rhs };
        },
        Call => {
            const paren = tree.nodes.items(.token)[index];
            const data: Node.Data = tree.nodes.items(.data)[index];
            const params = tree.extraData(SliceIndex, data.rhs);
            return Call{
                .paren = paren,
                .callee = data.lhs,
                .params = params,
            };
        },
        Get => {
            const name = tree.nodes.items(.token)[index];
            const data: Node.Data = tree.nodes.items(.data)[index];
            return Get{ .name = name, .obj = data.rhs };
        },

        else => @compileError(@typeName(T) ++ " not registered how to unpack"),
    }
}

pub const Super = Token;

pub const This = Token;

pub const Get = struct {
    name: Token,
    obj: Index,
};

pub const Call = struct {
    paren: Token,
    callee: Index,
    params: SliceIndex,
};

pub const Binary = struct {
    lhs: Index,
    rhs: Index,
    op: Token,
};

pub const Literal = Token;

pub const Unary = struct {
    op: Token,
    rhs: Index,
};

pub const FetchVar = Token;

pub const Assign = struct {
    name: Token,
    rhs: Index,
};

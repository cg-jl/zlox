const NodeResolver = @This();
const std = @import("std");
const context = @import("../context.zig");
const data = @import("data.zig");
const Token = @import("../Token.zig");

const Local = data.Local;
const Depth = data.Depth;

const AllocErr = std.mem.Allocator.Error;

const Ast = @import("../ast/Ast.zig");

pub fn resolveNode(r: *NodeResolver, node_index: Ast.Index) Result {
    const tag: Ast.Node.Tag = r.ast.nodes.items(.tag)[node_index];
    switch (tag) {
        .fetchVar => {
            const fetch_var = r.ast.unpack(Ast.FetchVar, node_index);
            if (r.latestScope()) |scope| {
                if (scope.get(fetch_var.source.lexeme)) |x| {
                    if (!x.initialized) {
                        context.reportSource(
                            fetch_var.source,
                            "Cannot read local variable in its own initializer",
                        );
                    }
                }
            }
            try r.resolveLocal(fetch_var);
        },
        .super => {
            const method = r.ast.unpack(Ast.Super, node_index);
            if (r.current_class == .none) {
                context.reportSource(
                    method.source,
                    "Cannot use 'super' outside of a class",
                );
            } else if (r.current_class != .subclass) {
                context.reportSource(
                    method.source,
                    "Cannot use 'super' in a class with no superclass",
                );
            } else {
                std.debug.assert(try r.resolveName("this", local(method)));
            }
        },
        .this => {
            const this = r.ast.unpack(Ast.This, node_index);
            if (r.current_class == .none) {
                context.reportSource(this.source, "Cannot use 'this' outside af a class");
            } else {
                std.debug.assert(try r.resolveName("this", local(this)));
            }
        },
        .function => {
            const func = r.ast.unpack(Ast.Function, node_index);
            try r.declare(func.name);
            try r.define(func.name);
            return r.resolveFuncDecl(func.decl, .function);
        },
        .literal,
        .naked_ret,
        => {},
        .unary => {
            const unary = r.ast.unpack(Ast.Rhs, node_index);
            return r.resolveNode(unary);
        },
        .binary => {
            const binary = r.ast.unpack(Ast.Node.Data, node_index);
            try r.resolveNode(binary.lhs);
            return r.resolveNode(binary.rhs);
        },
        .lambda => {
            const decl = r.ast.unpack(Ast.Node.FuncDecl, node_index);
            return r.resolveFuncDecl(decl, .function);
        },
        .assign => {
            const assign = r.ast.unpack(Ast.Assign, node_index);
            try r.resolveNode(assign.rhs);
            return r.resolveLocal(assign.name);
        },
        .call => {
            const call = r.ast.unpack(Ast.Call, node_index);
            try r.resolveNode(call.callee);
            for (call.params.start..call.params.end) |i| {
                try r.resolveNode(@intCast(Ast.Index, i));
            }
        },
        .get => {
            const rhs = r.ast.unpack(Ast.Rhs, node_index);
            return r.resolveNode(rhs);
        },
        .set => {
            const set = r.ast.unpack(Ast.Set, node_index);
            try r.resolveNode(set.value);
            return r.resolveNode(set.obj);
        },
        .if_simple => {
            const if_simple = r.ast.unpack(Ast.IfSimple, node_index);
            try r.resolveNode(if_simple.cond);
            return r.resolveNode(if_simple.then_branch);
        },
        .@"if" => {
            const full_if = r.ast.unpack(Ast.FullIf, node_index);
            try r.resolveNode(full_if.cond);
            try r.resolveNode(full_if.then_branch);
            return r.resolveNode(full_if.else_branch);
        },
        .print, .ret => {
            const value = r.ast.unpack(Ast.Rhs, node_index);
            return r.resolveNode(value);
        },
        .naked_while => {
            const body = r.ast.unpack(Ast.NakedWhile, node_index);
            return r.resolveNode(body);
        },
        .@"while" => {
            const info = r.ast.unpack(Ast.While, node_index);
            try r.resolveNode(info.cond);
            return r.resolveNode(info.body);
        },
        .naked_var_decl => {
            const name = r.ast.unpack(Ast.NakedVarDecl, node_index);
            try r.declare(name);
            return r.define(name);
        },
        .init_var_decl => {
            const init_var_decl = r.ast.unpack(Ast.InitVarDecl, node_index);
            try r.declare(init_var_decl.name);
            try r.resolveNode(init_var_decl.init);
            return r.define(init_var_decl.name);
        },
        .block => {
            const block = r.ast.unpack(Ast.Block, node_index);
            return r.resolveBlock(block);
        },
        .class => {
            const full_class = r.ast.unpack(Ast.FullClass, node_index);
            return r.resolveClass(
                full_class.name,
                full_class.methods,
                .subclass,
            );
        },
        .single_class => {
            const single_class = r.ast.unpack(Ast.SingleClass, node_index);
            return r.resolveClass(
                single_class.name,
                single_class.methods,
                .class,
            );
        },
    }
}

fn resolveClass(
    r: *NodeResolver,
    name: Token,
    methods: Ast.SliceIndex,
    ty: ClassType,
) Result {
    try r.declare(name);
    try r.define(name);

    const enclosing_class = r.current_class;
    defer r.current_class = enclosing_class;
    r.current_class = ty;

    try r.beginScope();
    defer r.endScope();
    try r.defineName("this");
    for (methods.start..methods.end) |i| {
        std.debug.assert(r.ast.nodes.items(.tag)[i] == .function);
        const method = r.ast.unpack(Ast.Function, i);
        const is_init = std.mem.eql(u8, "init", method.name.source.lexeme);
        const decltype: FuncType = if (is_init) .initializer else .method;
        try r.resolveFuncDecl(method.decl, decltype);
    }
}

fn resolveFuncDecl(
    r: *NodeResolver,
    decl: Ast.Node.FuncDecl,
    ty: FuncType,
) Result {
    const enclosing_function = r.current_function;
    defer r.current_function = enclosing_function;
    r.current_function = ty;

    try r.beginScope();
    defer r.endScope();

    for (decl.params.start..decl.params.end) |i| {
        const param = r.ast.unpack(Ast.IndexedToken, i).tok;
        try r.declare(param);
        try r.define(param);
    }

    // Do not create a new scope for these. This makes shadowing arguments
    // illegal, but they're mutable so it's OK.
    for (decl.body.start..decl.body.end) |i| {
        try r.resolveNode(@intCast(Ast.Index, i));
    }
}

inline fn resolveBlock(r: *NodeResolver, block: Ast.SliceIndex) Result {
    try r.beginScope();
    defer r.endScope();
    for (block.start..block.end) |i| {
        try r.resolveNode(@intCast(Ast.Index, i));
    }
}

const VarInfo = packed struct {
    initialized: bool,
    stack_index: u32,
};
pub inline fn local(token: Token) Local {
    return .{
        .line = token.source.line,
        .col = token.source.col,
    };
}

pub fn LocalMap(comptime T: type) type {
    return std.HashMapUnmanaged(
        Local,
        T,
        LocalContext,
        std.hash_map.default_max_load_percentage,
    );
}

pub const DepthMap = LocalMap(data.Depth);

const LocalContext = struct {
    pub fn eql(_: LocalContext, a: Local, b: Local) bool {
        return @bitCast(Local.U, a) == @bitCast(Local.U, b);
    }

    pub fn hash(_: LocalContext, k: Local) u64 {
        return @bitCast(Local.U, k);
    }
};

const Scope = std.StringHashMapUnmanaged(VarInfo);
const ClassType = enum(u2) { none, class, subclass };
const FuncType = enum(u2) { none, function, initializer, method };

const Result = data.AllocErr!void;

ast: Ast = undefined,
locals: DepthMap.Managed,
scopes: std.ArrayListUnmanaged(Scope) = .{},
current_function: FuncType = FuncType.none,
current_class: ClassType = ClassType.none,
arena: std.heap.ArenaAllocator,

pub fn init(alloc: std.mem.Allocator) !NodeResolver {
    var res = NodeResolver{
        .arena = std.heap.ArenaAllocator.init(alloc),
        .locals = DepthMap.Managed.init(alloc),
    };

    // global scope
    try res.beginScope();
    try res.defineName("clock");
    return res;
}

pub fn deinit(res: *NodeResolver) void {
    res.arena.deinit();
}

inline fn beginScope(r: *NodeResolver) AllocErr!void {
    return r.scopes.append(r.ally(), .{});
}

fn endScope(r: *NodeResolver) void {
    var env = r.scopes.pop();
    env.deinit(r.ally());
}

fn declare(r: *NodeResolver, name: Token) Result {
    const scope = r.latestScope().?;
    const get_or_put: Scope.GetOrPutResult = try scope.getOrPut(
        r.ally(),
        name.source.lexeme,
    );
    if (get_or_put.found_existing) {
        context.reportSource(
            name.source,
            "Variable with this name already declared in this scope",
        );
    }

    get_or_put.value_ptr.initialized = false;
    get_or_put.value_ptr.stack_index = scope.size - 1;
}

fn defineName(r: *NodeResolver, name: []const u8) AllocErr!void {
    const scope = r.latestScope().?;
    const get_or_put: Scope.GetOrPutResult = try scope.getOrPut(r.ally(), name);
    if (!get_or_put.found_existing) {
        get_or_put.value_ptr.stack_index = scope.size - 1;
    }
    get_or_put.value_ptr.initialized = true;
}

fn latestScope(r: *NodeResolver) ?*Scope {
    if (r.scopes.items.len == 0) return null;
    return &r.scopes.items[r.scopes.items.len - 1];
}

inline fn define(r: *NodeResolver, name: Token) AllocErr!void {
    return r.defineName(name.source.lexeme);
}
fn resolveName(r: *NodeResolver, name: []const u8, use_local: Local) data.AllocErr!bool {
    var i: usize = r.scopes.items.len;
    return while (i > 0) : (i -= 1) {
        const scope: *const Scope = &r.scopes.items[i - 1];
        if (scope.get(name)) |info| {
            try r.locals.put(use_local, .{
                .env = r.scopes.items.len - i,
                .stack = info.stack_index,
            });
            break true;
        }
    } else false;
}
fn resolveLocal(r: *NodeResolver, name: Token) data.AllocErr!void {
    const could_resolve_name = try r.resolveName(name.source.lexeme, local(name));
    if (!could_resolve_name)
        context.reportSource(name.source, "Could not resolve variable");
}

inline fn ally(r: *NodeResolver) std.mem.Allocator {
    return r.arena.allocator();
}

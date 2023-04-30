const NodeResolver = @This();
const std = @import("std");
const context = @import("../context.zig");
const data = @import("data.zig");

const Local = data.Local;
const Depth = data.Depth;

const AllocErr = std.mem.Allocator.Error;

const ast = @import("../ast.zig");

const VarInfo = packed struct {
    initialized: bool,
    stack_index: u32,
};
pub inline fn local(token: ast.Expr.Var) Local {
    return .{
        .line = token.line,
        .col = token.col,
    };
}

pub const LocalMap = std.HashMapUnmanaged(
    Local,
    Depth,
    LocalContext,
    std.hash_map.default_max_load_percentage,
);
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

ast: ast.Ast = undefined,
locals: LocalMap.Managed,
scopes: std.ArrayListUnmanaged(Scope) = .{},
current_function: FuncType = FuncType.none,
current_class: ClassType = ClassType.none,
arena: std.heap.ArenaAllocator,

pub fn init(alloc: std.mem.Allocator) !NodeResolver {
    var res = NodeResolver{
        .arena = std.heap.ArenaAllocator.init(alloc),
        .locals = LocalMap.Managed.init(alloc),
    };

    // global scope
    try res.beginScope();
    try res.defineName("clock");
    return res;
}

pub fn deinit(res: *NodeResolver) void {
    res.arena.deinit();
    res.locals.deinit();
}

fn beginScope(_: *NodeResolver) AllocErr!void {}
fn defineName(_: *NodeResolver, _: []const u8) AllocErr!void {}

pub fn resolveNode(_: *NodeResolver, _: ast.Ast.Index) Result {}

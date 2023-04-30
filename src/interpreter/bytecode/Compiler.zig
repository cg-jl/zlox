const Compiler = @This();
const ast = @import("../../ast.zig");
const Parser = @import("../../Parser.zig");
const std = @import("std");
const Resolver = @import("../Resolver.zig");

const bytecode = @import("../bytecode.zig");

opcodes: std.ArrayListUnmanaged(bytecode.OpCode) = .{},
constants: std.ArrayListUnmanaged(data.Value) = .{},
positions: std.ArrayListUnmanaged(Resolver.Local) = .{},
locals: Resolver.LocalMap,
alloc: std.mem.Allocator,

const WriteError = std.mem.Allocator.Error;

pub fn print(c: *Compiler) WriteError!void {
    try c.opcodes.append(c.alloc, .print);
}
pub fn ret(w: *Compiler) WriteError!void {
    try w.opcodes.append(w.alloc, .ret);
}
pub fn constant(w: *Compiler, value: data.Value) WriteError!void {
    try w.constants.append(w.alloc, value);
    try w.opcodes.append(w.alloc, .constant);
}

pub fn compileExpr(c: *Compiler, e: ast.Expr) WriteError!void {
    switch (e) {
        .literal => |l| {
            const value = switch (l) {
                .string => |s| .{ .string = .{ .string = s, .was_allocated = false } },
                .num => |n| .{ .num = n },
                .boolean => |b| .{ .boolean = b },
                .nil => .{ .nil = {} },
            };
            try c.constant(value);
            try c.positions.append(c.alloc, undefined);
        },
        .binary => |b| {
            const bin: ast.Expr.Binary = b;
            try c.compileExpr(bin.left.*);
            try c.compileExpr(bin.right.*);
            try c.positions.append(c.alloc, bin.operator);
            switch (bin.operator.ty) {
                .MINUS => try c.opcodes.append(c.alloc, .{ .sub = {} }),
                .SLASH => try c.opcodes.append(c.alloc, .{ .slash = {} }),
                .STAR => try c.opcodes.append(c.alloc, .{ .mul = {} }),
                .PLUS => try c.opcodes.append(c.alloc, .{ .add = {} }),
                .GREATER => try c.opcodes.append(c.alloc, .{ .gt = {} }),
                .LESS => try c.opcodes.append(c.alloc, .{ .lt = {} }),
                .GREATER_EQUAL => {
                    try c.positions.append(c.alloc, undefined);
                    const arr = try c.opcodes.addManyAsArray(c.alloc, 2);
                    arr.* = [2]bytecode.Op{ .{ .lt = {} }, .{ .not = {} } };
                },
                .LESS_EQUAL => {
                    try c.positions.append(c.alloc, undefined);
                    const arr = try c.opcodes.addManyAsArray(c.alloc, 2);
                    arr.* = [2]bytecode.Op{ .{ .gt = {} }, .{ .not = {} } };
                },
                else => unreachable,
            }
        },
        .unary => |u| {
            const un: ast.Expr.Unary = u;
            try c.compileExpr(un.right.*);

            try c.positions.append(un.operator);
            try c.opcodes.append(c.alloc, switch (un.operator.ty) {
                .MINUS => .{ .negate = {} },
                .BANG => .{ .not = {} },
                else => unreachable,
            });
        },
        .@"var" => |v| {
            const name: ast.Expr.Var = v;
            const depth = c.locals.get(Resolver.local(name)).?;
            try c.positions.append(c.alloc, name);
            try c.opcodes.append(c.alloc, .{ .local = depth });
        },
        .this => |t| {
            const depth = c.locals.get(Resolver.local(t)).?;
            try c.positions.append(c.alloc, t);
            try c.opcodes.append(c.alloc, .{ .local = depth });
        },
        .super => |sp| {
            const super: ast.Expr.Super = sp;
            const super_depth = c.locals.get(Resolver.local(super.keyword)).?;
            const this_depth = data.Depth{ .env = super_depth.env - 1, .stack = 0 };
            comptime const extra_push_count = 3;
            const opcodes = try c.opcodes.addManyAsArray(c.alloc, extra_push_count);
            const positions = try c.positions.addManyAsArray(c.alloc, extra_push_count);
            opcodes[0] = .{ .local = this_depth };
            opcodes[1] = .{ .local = super_depth };
            opcodes[2] = .{ .super_method = super.method.lexeme };
            positions[0] = undefined;
            positions[1] = super.keyword;
            positions[2] = super.method;
        },
    }
}

pub fn compileStmt(c: *Compiler, s: ast.Stmt) WriteError!void {
    switch (s) {
        .print => |e| {
            try c.compileExpr(e);
            try c.print();
        },
        else => {},
    }
}

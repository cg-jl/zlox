//! Helper methods to build up the AST.
//! There is only one method to obtain pointers, which is `expandLifetime`.
//! The user is expected to pass in an arena allocator and make the pointers
//! have more lifetime when explicitly needed.
const ast = @import("../ast.zig");
const std = @import("std");
const Token = @import("../Token.zig");

const Expr = ast.Expr;
const Stmt = ast.Stmt;
const Error = std.mem.Allocator.Error;

const Builder = @This();

arena: std.heap.ArenaAllocator,

pub fn init(gpa: std.mem.Allocator) Builder {
    return .{ .arena = std.heap.ArenaAllocator.init(gpa) };
}

pub fn deinit(b: Builder) void {
    b.arena.deinit();
}

pub fn expandLifetimes(
    builder: *Builder,
    comptime T: type,
    values: []const T,
) Error![]const T {
    const arr = try builder.arena.allocator().alloc(T, values.len);
    std.mem.copy(T, arr, values);
    return arr;
}

pub fn expandLifetime(builder: *Builder, value: anytype) Error!*const @TypeOf(value) {
    const p = try builder.arena.allocator().create(@TypeOf(value));
    p.* = value;
    return p;
}

pub fn List(comptime T: type) type {
    return struct {
        inner: std.ArrayList(T),

        const List = @This();

        pub fn init(b: *const Builder) List {
            return .{ .inner = std.ArrayList(T).init(b.arena.allocator()) };
        }

        pub fn push(self: *List, v: T) Error!void {
            try self.inner.append(v);
        }

        pub fn drop(self: List) void {
            self.inner.deinit();
        }

        /// Releases the array, leaking the pointer.
        pub fn release(self: List) []const T {
            self.inner.shrinkAndFree(self.inner.items.len);
            return self.inner.items;
        }
    };
}

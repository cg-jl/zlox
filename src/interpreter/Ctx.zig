const std = @import("std");
const Ctx = @This();
const Error = @import("data.zig").Error;
const Token = @import("../Token.zig");

last_error: ?Error = null,
str_alloc: std.heap.ArenaAllocator,

pub inline fn init(gpa: std.mem.Allocator) Ctx {
    return .{ .str_alloc = std.heap.ArenaAllocator.init(gpa) };
}

pub inline fn deinit(ctx: Ctx) void {
    ctx.str_alloc.deinit();
}

pub inline fn ally(ctx: *Ctx) std.mem.Allocator {
    return ctx.str_alloc.allocator();
}

pub inline fn report(
    ctx: *Ctx,
    at: Token.Source,
    message: []const u8,
) void {
    ctx.last_error = .{ .source = at, .message = message };
}

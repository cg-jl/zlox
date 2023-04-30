const std = @import("std");
const Token = @import("Token.zig");

pub var has_errored: bool = false;

fn setErrored() void {
    has_errored = true;
}

pub fn clear() void {
    has_errored = false;
}

pub fn reportToken(token: Token, message: []const u8) void {
    setErrored();
    if (token.ty == .EOF) {
        std.log.err("{}:{}: Error at end: {s}", .{ token.line, token.col, message });
    } else {
        std.log.err("{}:{} Error at '{s}': {s}", .{
            token.line,
            token.col,
            token.lexeme,
            message,
        });
    }
}

pub fn report(line: u32, where: []const u8, comptime message: []const u8) void {
    std.log.err("[line {}] Error{s}: {s}", .{ line, where, message });
    setErrored();
}

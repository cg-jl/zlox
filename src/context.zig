const std = @import("std");
const Token = @import("Token.zig");

pub var has_errored: bool = false;

fn setErrored() void {
    has_errored = true;
}

pub fn clear() void {
    has_errored = false;
}

pub fn reportSource(src: Token.Source, message: []const u8) void {
    setErrored();
    const is_eof = src.lexeme.len == 0;
    if (is_eof) {
        std.log.err("{}:{}: Error at end: {s}", .{
            src.line,
            src.col,
            message,
        });
    } else {
        std.log.err("{}:{}: Error at '{s}': {s}", .{
            src.line,
            src.col,
            src.lexeme,
            message,
        });
    }
}

pub fn report(line: u32, where: []const u8, comptime message: []const u8) void {
    std.log.err("[line {}] Error{s}: {s}", .{ line, where, message });
    setErrored();
}

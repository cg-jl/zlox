const std = @import("std");

var has_errored: bool = false;

fn setErrored() void {
    has_errored = true;
}

fn clear() void {
    has_errored = false;
}

pub fn report(line: u32, comptime where: []const u8, comptime message: []const u8) void {
    std.log.err("[line {}] Error{s}: {s}", .{ line, where, message });
    setErrored();
}

const std = @import("std");

pub var has_errored: bool = false;

fn setErrored() void {
    has_errored = true;
}

pub fn clear() void {
    has_errored = false;
}

pub fn report(line: u32, where: []const u8, comptime message: []const u8) void {
    std.log.err("[line {}] Error{s}: {s}", .{ line, where, message });
    setErrored();
}

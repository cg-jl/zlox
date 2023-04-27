const std = @import("std");
const data = @import("data.zig");
const Env = @This();
const Alloc = std.mem.Allocator;
const Ctx = @import("Ctx.zig");
const Token = @import("../Token.zig");
const State = @import("State.zig");

values_begin: usize,
enclosing: ?*const Env,


pub fn ancestor(e: *const Env, distance: usize) *const Env {
    var curr = e;
    var i: usize = 0;
    while (i < distance) : (i += 1) {
        curr = curr.enclosing.?;
    }
    return curr;
}

fn makeTab(t: usize) void {
    var i: usize = 0;
    while (i < t) : (i += 1) {
        std.debug.print("  ", .{});
    }
}

pub fn print(e: *const Env, tab: usize) void {
    if (e.enclosing) |a| a.print(tab + 1);
    var it = e.values.iterator();
    while (it.next()) |entry| {
        makeTab(tab);
        std.debug.print("{s}: {}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
    }
}

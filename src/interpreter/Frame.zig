const std = @import("std");
const data = @import("data.zig");
const Frame = @This();
const Alloc = std.mem.Allocator;
const Ctx = @import("Ctx.zig");
const Token = @import("../Token.zig");
const State = @import("State.zig");

values_begin: usize,
enclosing: ?*const Frame,

pub fn ancestor(e: *const Frame, distance: usize) *const Frame {
    var curr = e;
    var i: usize = 0;
    while (i < distance) : (i += 1) {
        curr = curr.enclosing.?;
    }
    return curr;
}

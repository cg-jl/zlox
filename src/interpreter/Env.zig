const std = @import("std");
const data = @import("data.zig");
const Env = @This();
const Alloc = std.mem.Allocator;
const Ctx = @import("Ctx.zig");
const Token = @import("../Token.zig");

enclosing: ?*Env,
values: std.ArrayListUnmanaged(data.Value) = .{},

pub fn ancestor(e: *Env, distance: usize) ?*Env {
    var curr = e;
    var i: usize = 0;
    while (i < distance) : (i += 1) {
        curr = curr.enclosing orelse return null;
    }
    return curr;
}

pub inline fn assignAt(
    e: *Env,
    distance: usize,
    index: u32,
    value: data.Value,
) void {
    const env = e.ancestor(distance) orelse @panic("Must have ancestor");
    env.values.items[index] = value;
}

pub inline fn getAt(
    e: *Env,
    distance: usize,
    index: u32,
) data.Value {
    const env = e.ancestor(distance) orelse unreachable; // Java version would *nullptr
    return env.values.items[index];
}

pub fn assign(
    e: *Env,
    name: Token,
    value: data.Value,
    ctx: *Ctx,
) data.AllocOrSignal!void {
    var curr = e;
    while (true) {
        if (curr.values.getEntry(name.lexeme)) |ent| {
            ent.value_ptr.* = value;
            break;
        }
        curr = curr.enclosing orelse {
            ctx.report(name, try std.fmt.allocPrint(
                ctx.ally(),
                "Undefined variable '{s}'",
                .{name.lexeme},
            ));
            return error.RuntimeError;
        };
    }
}

pub fn get(
    e: *Env,
    name: Token,
    ctx: *Ctx,
) data.AllocOrSignal!data.Value {
    var curr = e;
    while (true) {
        if (curr.values.get(name.lexeme)) |v| return v;
        curr = curr.enclosing orelse {
            ctx.report(name, try std.fmt.allocPrint(
                ctx.ally(),
                "Undefined variable '{s}'",
                .{name.lexeme},
            ));
            return error.RuntimeError;
        };
    }
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

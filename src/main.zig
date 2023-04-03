const std = @import("std");
const Context = @import("context.zig");
const Scanner = @import("Scanner.zig");
const Token = @import("Token.zig");

pub fn main() !u8 {
    if (std.os.argv.len > 2) {
        std.log.err("Usage: zlox [script]", .{});
        return 64;
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    try Scanner.initKeywords(gpa.allocator());
    defer Scanner.deinitKeywords(gpa.allocator());


    if (std.os.argv.len == 2) {
        try runFile(std.mem.span(std.os.argv[1]), gpa.allocator());
    } else {
        try runPrompt(gpa.allocator());
    }
    return 0;
}

fn run(toks: []const Token) void {
    for (toks) |tk| {
        std.log.debug("{}", .{tk});
    }
}

fn runFile(filename: []const u8, gpa: std.mem.Allocator) !void {
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();
    const stat = try file.stat();
    const mem = try std.os.mmap(null, stat.size, std.os.PROT.READ, std.os.MAP.SHARED, file.handle, 0);
    defer std.os.munmap(mem);

    var tokens = std.ArrayList(Token).init(gpa);
    defer tokens.deinit();
    try Scanner.scan(&tokens, mem);
    run(tokens.items);
}

fn runPrompt(gpa: std.mem.Allocator) !void {
    var buf_reader = std.io.bufferedReader(std.io.getStdIn().reader());
    var input = buf_reader.reader();

    // Allocate enough for a line
    var variable_input = try std.ArrayListUnmanaged(u8).initCapacity(gpa, 128);
    defer variable_input.deinit(gpa);

    var tokens = std.ArrayListUnmanaged(Token){};
    defer tokens.deinit(gpa);

    _ = std.io.getStdOut().writer().write("> ") catch {};

    while (true) {
        const line_end =
            input.readUntilDelimiter(variable_input.unusedCapacitySlice(), '\n') catch |err|
            {
            // Too small of a buffer (unlikely)
            if (err == error.StreamTooLong) {
                try variable_input.ensureUnusedCapacity(gpa, variable_input.unusedCapacitySlice().len * 2);
                continue;
            }
            // EOF
            if (err == error.EndOfStream) {
                break;
            }
            return err;
        };

        variable_input.items.len += line_end.len;
        tokens.clearRetainingCapacity();
        var toks_managed = tokens.toManaged(gpa);
        defer tokens = toks_managed.moveToUnmanaged();
        try Scanner.scan(&toks_managed, variable_input.items);
        variable_input.clearRetainingCapacity();
        run(toks_managed.items);
        _ = std.io.getStdOut().writer().write("> ") catch {};
    }
}

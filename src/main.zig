const std = @import("std");
const Context = @import("context.zig");
const Scanner = @import("Scanner.zig");
const Token = @import("Token.zig");
const Resolver = @import("interpreter/Resolver.zig");
const Walker = @import("interpreter/Walker.zig");
const Parser = @import("Parser.zig");
const ast = @import("ast.zig");

pub fn main() !u8 {
    if (std.os.argv.len > 2) {
        std.log.err("Usage: zlox [script]", .{});
        return 64;
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{
        .enable_memory_limit = true,
    }){
        .requested_memory_limit = 4 << 30, // no more than 4GiB
    };
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

fn runFile(filename: []const u8, gpa: std.mem.Allocator) !void {
    std.log.debug("runningn {s}", .{filename});
    const file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();
    const stat = try file.stat();
    const mem = try std.os.mmap(null, stat.size, std.os.PROT.READ, std.os.MAP.SHARED, file.handle, 0);
    defer std.os.munmap(mem);

    var tokens = std.ArrayList(Token).init(gpa);
    defer tokens.deinit();
    try Scanner.scan(&tokens, mem);
    if (Context.has_errored) return;
    var builder = ast.Builder.init(gpa);
    defer builder.deinit();
    // use an arena for all the strings, so we can free them
    // all at once. Better allocator for this?
    var str_alloc = std.heap.ArenaAllocator.init(gpa);
    defer str_alloc.deinit();
    var parser = Parser.init(tokens.items, &builder, str_alloc.allocator());
    var stmts = std.ArrayList(ast.Stmt).init(gpa);
    defer stmts.deinit();
    try parser.parse(&stmts);
    if (Context.has_errored) return;

    var state = try Walker.initCore(gpa);
    defer state.core.deinit();

    // Make sure to free resolver's memory
    {
        var resolver = try Resolver.init(gpa);
        defer resolver.deinit();
        try resolver.resolveBlock(stmts.items);
        state.locals = resolver.locals.unmanaged;
    }
    defer state.locals.deinit(gpa);

    if (Context.has_errored) return;

    try state.tryExecBlock(stmts.items);
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

    var builder = ast.Builder.init(gpa);
    var gp_arena = std.heap.ArenaAllocator.init(gpa);
    var state = try Walker.initCore(gpa);
    defer state.core.deinit();

    var resolver = try Resolver.init(gpa);
    defer resolver.deinit();
    defer resolver.locals.deinit();

    defer builder.deinit();
    defer gp_arena.deinit();

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
        defer {
            variable_input.clearRetainingCapacity();
            _ = std.io.getStdOut().writer().write("> ") catch {};
        }
        Context.clear();
        tokens.clearRetainingCapacity();
        {
            var toks_managed = tokens.toManaged(gpa);
            defer tokens = toks_managed.moveToUnmanaged();
            try Scanner.scan(&toks_managed, variable_input.items);
        }
        if (Context.has_errored) continue;

        defer _ = builder.arena.reset(.retain_capacity);
        defer _ = gp_arena.reset(.retain_capacity);
        var parser = Parser.init(tokens.items, &builder, gp_arena.allocator());

        if (hasStatementShit(tokens.items)) {
            var stmts = std.ArrayList(ast.Stmt).init(gp_arena.allocator());
            try parser.parse(&stmts);
            if (Context.has_errored) continue;
            try resolver.resolveBlock(stmts.items);
            if (Context.has_errored) continue;
            state.locals = resolver.locals.unmanaged;
            try state.tryExecBlock(stmts.items);
        } else {
            const expr = try parser.tryExpression();
            if (expr) |e| {
                try resolver.resolveExpr(e);
                if (Context.has_errored) continue;
                state.locals = resolver.locals.unmanaged;
                try state.tryPrintExpr(e);
            }
        }
    }
}

fn hasStatementShit(tokens: []const Token) bool {
    const stmt_tokens = comptime &.{ .IF, .WHILE, .FOR, .ELSE, .CLASS, .VAR, .FUN, .RETURN };
    const mask = comptime Token.Mask.initMany(stmt_tokens);

    for (tokens) |tk| {
        if (mask.contains(tk.ty)) return true;
    }
    return false;
}

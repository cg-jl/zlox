const std = @import("std");
const Context = @import("context.zig");
const Scanner = @import("Scanner.zig");
const Token = @import("Token.zig");
const Resolver = @import("interpreter/NodeResolver.zig");
const Walker = @import("interpreter/NodeWalker.zig");
const Parser = @import("NodeParser.zig");
const Builder = @import("ast/NodeBuilder.zig");
const Ast = @import("ast/Ast.zig");
const callgrind = std.valgrind.callgrind;

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
    var builder = Builder{ .alloc = gpa };
    defer {
        builder.extra_data.deinit(builder.alloc);
        builder.node_list.deinit(builder.alloc);
        builder.annotated_tokens.deinit(builder.alloc);
    }
    // use an arena for all the strings, so we can free them
    // all at once. Better allocator for this?
    var str_alloc = std.heap.ArenaAllocator.init(gpa);
    defer str_alloc.deinit();
    var parser = Parser{
        .tokens = tokens.items,
        .builder = &builder,
        .temp_node_allocator = gpa,
    };
    var root = std.ArrayList(Ast.Index).init(gpa);
    defer root.deinit();
    const parsed_ast = try parser.parse(&root);
    if (Context.has_errored) return;

    var state = try Walker.initCore(gpa, parsed_ast);
    defer state.core.deinit();

    // Make sure to free resolver's memory
    {
        var resolver = try Resolver.init(gpa);
        resolver.ast = parsed_ast;
        defer resolver.deinit();
        for (root.items) |i| try resolver.resolveNode(i);
        state.locals = resolver.locals.unmanaged;
    }
    defer state.locals.deinit(gpa);

    state.ast = parsed_ast;

    if (Context.has_errored) return;

    callgrind.startInstrumentation();
    try state.tryVisitBlock(root.items);
    callgrind.stopInstrumentation();
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

    var gp_arena = std.heap.ArenaAllocator.init(gpa);
    var builder = Builder{ .alloc = gp_arena.allocator() };
    var state = try Walker.initCore(gpa, undefined);
    defer state.core.deinit();

    var resolver = try Resolver.init(gpa);
    defer resolver.deinit();
    defer resolver.locals.deinit();

    defer gp_arena.deinit();
    defer builder.deinit();

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

        defer _ = gp_arena.reset(.retain_capacity);
        var parser = Parser{
            .tokens = tokens.items,
            .builder = &builder,
            .temp_node_allocator = gp_arena.allocator(),
        };

        const stmt = try parser.tryDeclaration(false) orelse continue;
        defer {
            parser.builder.extra_data.clearRetainingCapacity();
            parser.builder.annotated_tokens.clearRetainingCapacity();
            parser.builder.node_list.len = 0;
        }
        const parsed_ast = parser.ast();
        resolver.ast = parsed_ast;
        try resolver.resolveNode(stmt);
        if (Context.has_errored) continue;

        state.locals = resolver.locals.unmanaged;
        state.ast = parsed_ast;
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

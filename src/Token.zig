const std = @import("std");

const Token = @This();

pub const Ty = enum(u6) {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    IDENTIFIER,
    STRING,
    NUMBER,
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
    EOF,
};

pub const Mask = std.enums.EnumSet(Ty);

pub const TaggedLiteral = union(enum(u2)) {
    string: []const u8,
    num: f64,
    boolean: bool,
    nil: void,
};

pub const Literal = union {
    string: []const u8,
    num: f64,
    none: void,
};

ty: Ty,
lexeme: []const u8,
literal: Literal,
line: u32,

pub fn extractLiteral(tok: Token) TaggedLiteral {
    return switch (tok.ty) {
        .NUMBER => .{ .num = tok.literal.num },
        .STRING => .{ .string = tok.literal.string },
        .TRUE => .{ .boolean = true },
        .FALSE => .{ .boolean = false },
        .NIL => .{ .nil = {} },
        else => unreachable,
    };
}

pub fn format(
    slf: Token,
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    try writer.print("{} {s}", .{ slf.ty, slf.lexeme });
    if (slf.ty == .STRING) {
        try writer.print(" {s}", .{slf.literal.string});
    } else if (slf.ty == .NUMBER) {
        try writer.print(" {}", .{slf.literal.num});
    }
}

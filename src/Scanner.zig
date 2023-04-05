const Token = @import("Token.zig");
const std = @import("std");
const AllocError = std.mem.Allocator.Error;
const ctx = @import("context.zig");

const Tokens = std.ArrayList(Token);

var keywords: std.StringHashMapUnmanaged(Token.Ty) = undefined;

pub fn initKeywords(gpa: std.mem.Allocator) !void {
    keywords = std.StringHashMapUnmanaged(Token.Ty){};

    try keywords.ensureTotalCapacity(gpa, 16);
    try keywords.put(gpa, "and", .AND);
    try keywords.put(gpa, "class", .CLASS);
    try keywords.put(gpa, "else", .ELSE);
    try keywords.put(gpa, "false", .FALSE);
    try keywords.put(gpa, "fun", .FUN);
    try keywords.put(gpa, "for", .FOR);
    try keywords.put(gpa, "if", .IF);
    try keywords.put(gpa, "nil", .NIL);
    try keywords.put(gpa, "or", .OR);
    try keywords.put(gpa, "print", .PRINT);
    try keywords.put(gpa, "return", .RETURN);
    try keywords.put(gpa, "super", .SUPER);
    try keywords.put(gpa, "this", .THIS);
    try keywords.put(gpa, "true", .TRUE);
    try keywords.put(gpa, "var", .VAR);
    try keywords.put(gpa, "while", .WHILE);
}

pub fn deinitKeywords(keywords_gpa: std.mem.Allocator) void {
    keywords.deinit(keywords_gpa);
}

const Scanner = @This();

source: []const u8,
start: u32 = 0,
current: u32 = 0,
line: u32 = 1,

pub fn init(source: []const u8) Scanner {
    return .{
        .source = source,
    };
}

pub fn scan(tokens: *Tokens, full_source: []const u8) AllocError!void {
    var scanner = Scanner.init(full_source);
    try scanner.scanTokens(tokens);
}

pub fn scanTokens(scn: *Scanner, tokens: *Tokens) AllocError!void {
    while (!scn.isAtEnd()) {
        scn.start = scn.current;
        try scn.scanToken(tokens);
    }
    try tokens.append(Token{
        .ty = .EOF,
        .lexeme = "",
        .literal = .{ .none = {} },
        .line = scn.line,
    });
}

fn isAtEnd(scn: *const Scanner) bool {
    return scn.current >= scn.source.len;
}

fn scanToken(scn: *Scanner, tokens: *Tokens) AllocError!void {
    const c: u8 = scn.advance();
    switch (c) {
        '(' => return try tokens.append(scn.tokenFromTy(.LEFT_PAREN)),
        ')' => return try tokens.append(scn.tokenFromTy(.RIGHT_PAREN)),
        '{' => return try tokens.append(scn.tokenFromTy(.LEFT_BRACE)),
        '}' => return try tokens.append(scn.tokenFromTy(.RIGHT_BRACE)),
        ',' => return try tokens.append(scn.tokenFromTy(.COMMA)),
        '.' => return try tokens.append(scn.tokenFromTy(.DOT)),
        '-' => return try tokens.append(scn.tokenFromTy(.MINUS)),
        '+' => return try tokens.append(scn.tokenFromTy(.PLUS)),
        ';' => return try tokens.append(scn.tokenFromTy(.SEMICOLON)),
        '*' => return try tokens.append(scn.tokenFromTy(.STAR)),
        '!' => return try tokens.append(scn.tokenFromTy(
            if (scn.match('='))
                .BANG_EQUAL
            else
                .BANG,
        )),
        '=' => return try tokens.append(scn.tokenFromTy(
            if (scn.match('='))
                .EQUAL_EQUAL
            else
                .EQUAL,
        )),
        '<' => return try tokens.append(scn.tokenFromTy(
            if (scn.match('='))
                .LESS_EQUAL
            else
                .LESS,
        )),
        '>' => return try tokens.append(scn.tokenFromTy(
            if (scn.match('='))
                .GREATER_EQUAL
            else
                .GREATER,
        )),
        '/' => {
            if (scn.match('/')) {
                scn.current += 1;
                while (!scn.isAtEnd() and scn.peek() != '\n') scn.current += 1;
            } else {
                return try tokens.append(scn.tokenFromTy(.SLASH));
            }
        },
        ' ', '\r', '\t' => {},
        '\n' => {
            scn.line += 1;
        },
        '"' => return try scn.string(tokens),
        else => {
            if (std.ascii.isDigit(c)) {
                return try scn.number(tokens);
            } else if (std.ascii.isAlphabetic(c)) {
                return try scn.identifier(tokens);
            } else {
                scn.report("Unexpected character");
            }
        },
    }
}

fn identifier(scn: *Scanner, tokens: *Tokens) !void {
    while (!scn.isAtEnd() and std.ascii.isAlphanumeric(scn.peek())) {
        scn.current += 1;
    }

    const text = scn.source[scn.start..scn.current];
    const typ = keywords.get(text) orelse .IDENTIFIER;
    try tokens.append(scn.tokenFromTy(typ));
}

fn number(scn: *Scanner, tokens: *Tokens) !void {
    while (!scn.isAtEnd() and std.ascii.isDigit(scn.peek())) {
        scn.current += 1;
    }

    if (scn.current + 1 < scn.source.len and scn.peek() == '.' and
        std.ascii.isDigit(scn.source[scn.current + 1]))
    {
        scn.current += 2;
        while (!scn.isAtEnd() and std.ascii.isDigit(scn.peek())) {
            scn.current += 1;
        }
    }

    const value = std.fmt.parseFloat(f64, scn.source[scn.start..scn.current]) catch unreachable;
    try tokens.append(scn.token(.NUMBER, .{ .num = value }));
}

fn string(scn: *Scanner, tokens: *Tokens) !void {
    while (!scn.isAtEnd() and scn.peek() != '"') {
        if (scn.peek() == '\n') scn.line += 1;
        scn.current += 1;
    }

    if (scn.isAtEnd()) {
        scn.report("Unterminated string");
        return;
    }
    scn.current += 1;

    const value = scn.source[scn.start + 1 .. scn.current - 1];
    try tokens.append(scn.token(.STRING, .{ .string = value }));
}

fn report(scn: *const Scanner, comptime message: []const u8) void {
    ctx.report(scn.line, "", message);
}

fn match(scn: *Scanner, comptime expect: u8) bool {
    if (scn.isAtEnd()) return false;
    if (scn.peek() != expect) return false;
    scn.current += 1;
    return true;
}

fn peek(scn: *const Scanner) u8 {
    return scn.source[scn.current];
}

fn advance(scn: *Scanner) u8 {
    const ch = scn.source[scn.current];
    scn.current += 1;
    return ch;
}

fn token(scn: *const Scanner, ty: Token.Ty, lit: Token.Literal) Token {
    return Token{
        .line = scn.line,
        .lexeme = scn.source[scn.start..scn.current],
        .ty = ty,
        .literal = lit,
    };
}

fn tokenFromTy(scn: *const Scanner, ty: Token.Ty) Token {
    return scn.token(ty, .{ .none = {} });
}

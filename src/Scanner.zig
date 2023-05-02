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
line_start: u32 = 0,
start: u32 = 0,
current: u32 = 0,
line: u16 = 1,

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
        .source = scn.grabSource(),
    });
}

fn isAtEnd(scn: *const Scanner) bool {
    return scn.current >= scn.source.len;
}

fn scanToken(scn: *Scanner, tokens: *Tokens) AllocError!void {
    const c: u8 = scn.advance();
    switch (c) {
        '(' => return tokens.append(scn.token(.LEFT_PAREN)),
        ')' => return tokens.append(scn.token(.RIGHT_PAREN)),
        '{' => return tokens.append(scn.token(.LEFT_BRACE)),
        '}' => return tokens.append(scn.token(.RIGHT_BRACE)),
        ',' => return tokens.append(scn.token(.COMMA)),
        '.' => return tokens.append(scn.token(.DOT)),
        '-' => return tokens.append(scn.token(.MINUS)),
        '+' => return tokens.append(scn.token(.PLUS)),
        ';' => return tokens.append(scn.token(.SEMICOLON)),
        '*' => return tokens.append(scn.token(.STAR)),
        '!' => return tokens.append(scn.token(
            if (scn.match('='))
                .BANG_EQUAL
            else
                .BANG,
        )),
        '=' => return tokens.append(scn.token(
            if (scn.match('='))
                .EQUAL_EQUAL
            else
                .EQUAL,
        )),
        '<' => return tokens.append(scn.token(
            if (scn.match('='))
                .LESS_EQUAL
            else
                .LESS,
        )),
        '>' => return tokens.append(scn.token(
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
                return tokens.append(scn.token(.SLASH));
            }
        },
        ' ', '\r', '\t' => {},
        '\n' => {
            scn.advanceLine();
        },
        '"' => return scn.string(tokens),
        else => {
            if (std.ascii.isDigit(c)) {
                return scn.number(tokens);
            } else if (isIdentStart(c)) {
                return scn.identifier(tokens);
            } else {
                scn.report("Unexpected character");
            }
        },
    }
}

fn isIdentStart(ch: u8) bool {
    return ch == '_' or std.ascii.isAlphabetic(ch);
}

fn isIdent(ch: u8) bool {
    return ch == '_' or std.ascii.isAlphanumeric(ch);
}

fn identifier(scn: *Scanner, tokens: *Tokens) !void {
    while (!scn.isAtEnd() and isIdent(scn.peek())) {
        scn.current += 1;
    }

    const text = scn.source[scn.start..scn.current];
    const typ = keywords.get(text) orelse .IDENTIFIER;
    try tokens.append(scn.token(typ));
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

    try tokens.append(scn.token(.NUMBER));
}

fn string(scn: *Scanner, tokens: *Tokens) !void {
    while (!scn.isAtEnd() and scn.peek() != '"') {
        if (scn.peek() == '\n') scn.advanceLine();
        scn.current += 1;
    }

    if (scn.isAtEnd()) {
        scn.report("Unterminated string");
        return;
    }
    scn.current += 1;

    try tokens.append(scn.token(.STRING));
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

fn advanceLine(scn: *Scanner) void {
    scn.line += 1;
    scn.line_start = scn.current;
}

fn token(scn: *const Scanner, ty: Token.Ty) Token {
    return Token{
        .source = scn.grabSource(),
        .ty = ty,
    };
}

fn grabSource(scn: *const Scanner) Token.Source {
    return Token.Source{
        .lexeme = scn.source[scn.start..scn.current],
        .col = scn.column(),
        .line = scn.line,
    };
}

inline fn column(scn: *const Scanner) u32 {
    return scn.current - scn.line_start;
}

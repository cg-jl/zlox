const Walker = @This();
const context = @import("../context.zig");
const Core = @import("Core.zig");
const Resolver = @import("Resolver.zig");
const ast = @import("../ast.zig");
const data = @import("data.zig");
const std = @import("std");
const Token = @import("../Token.zig");
const Frame = @import("Frame.zig");

const local = Resolver.local;

const AllocErr = std.mem.Allocator.Error;

core: Core,
// This is taking a lot of hits, even if it's just storing positions and their
// respective code locations.
locals: Resolver.LocalMap,

pub fn initCore(gpa: std.mem.Allocator) !Walker {
    var core = try Core.init(gpa, 1);
    core.values.appendAssumeCapacity(
        .{ .callable = data.CallableVT{
            .ptr = undefined,
            .repr = "<native fn clock()>",
            .call = &getClock,
            .arity = 0,
        } },
    );

    return Walker{ .core = core, .locals = undefined };
}

inline fn visitBinary(state: *Walker, b: ast.Expr.Binary) data.Result {
    const left = try state.visitExpr(b.left.*);
    const right = try state.visitExpr(b.right.*);

    switch (b.operator.ty) {
        .MINUS => {
            const checked = try state.checkNumberOperands(b.operator, left, right);
            return .{ .num = checked.left - checked.right };
        },
        .SLASH => {
            const checked = try state.checkNumberOperands(b.operator, left, right);
            return .{ .num = checked.left / checked.right };
        },
        .STAR => {
            const checked = try state.checkNumberOperands(b.operator, left, right);
            return .{ .num = checked.left * checked.right };
        },
        .PLUS => {
            switch (left) {
                .num => |ln| switch (right) {
                    .num => |rn| return .{ .num = ln + rn },
                    else => {},
                },
                .string => |ls| switch (right) {
                    .string => |rs| return .{
                        .string = .{
                            .string = try std.fmt.allocPrint(
                                state.core.ctx.ally(),
                                "{s}{s}",
                                .{ ls.string, rs.string },
                            ),
                            .alloc_refcount = 0,
                        },
                    },
                    else => {},
                },
                else => {},
            }

            state.core.ctx.report(b.operator, "Operands must be two numbers or two strings");
            return error.RuntimeError;
        },
        .GREATER => {
            const checked = try state.checkNumberOperands(b.operator, left, right);
            return .{ .boolean = checked.left > checked.right };
        },
        .GREATER_EQUAL => {
            const checked = try state.checkNumberOperands(b.operator, left, right);
            return .{ .boolean = checked.left >= checked.right };
        },
        .LESS => {
            const checked = try state.checkNumberOperands(b.operator, left, right);
            return .{ .boolean = checked.left < checked.right };
        },
        .LESS_EQUAL => {
            const checked = try state.checkNumberOperands(b.operator, left, right);
            return .{ .boolean = checked.left <= checked.right };
        },

        // unknown op
        else => unreachable,
    }
}

fn checkNumberOperands(
    state: *Walker,
    op: Token,
    left: data.Value,
    right: data.Value,
) data.Signal!struct { left: f64, right: f64 } {
    switch (left) {
        .num => |ln| {
            switch (right) {
                .num => |rn| return .{ .left = ln, .right = rn },
                else => {},
            }
        },
        else => {},
    }
    state.core.ctx.report(op, "Operands must be numbers");
    return error.RuntimeError;
}
pub fn tryPrintExpr(state: *Walker, e: ast.Expr) std.mem.Allocator.Error!void {
    const v = state.visitExpr(e) catch |err| {
        switch (err) {
            error.Return => {
                std.log.err("Caught return in global context", .{});
                context.has_errored = true;
                return;
            },
            error.RuntimeError => {
                const a: data.Error = state.core.ctx.last_error orelse unreachable;
                std.log.err("@ line {} '{s}': {s}", .{ a.token.line, a.token.lexeme, a.message });
                context.has_errored = true;
                return;
            },
            else => return @errSetCast(AllocErr, err),
        }
    };

    std.debug.print("{}\n", .{v});
}

pub fn tryExecBlock(state: *Walker, stmts: []const ast.Stmt) std.mem.Allocator.Error!void {
    var frame: Frame = undefined;
    state.core.pushFrame(&frame);
    defer state.core.restoreFrame(frame);
    state.core.current_env.enclosing = &frame;
    state.executeBlock(stmts) catch |err| {
        switch (err) {
            error.Return => {
                std.log.err("Caught return in global context", .{});
                context.has_errored = true;
                return;
            },
            error.RuntimeError => {
                const a: data.Error = state.core.ctx.last_error orelse unreachable;
                std.log.err("@ line {} '{s}': {s}", .{ a.token.line, a.token.lexeme, a.message });
                context.has_errored = true;
                return;
            },
            else => return @errSetCast(AllocErr, err),
        }
    };
}

pub fn visitStmt(state: *Walker, st: ast.Stmt) data.VoidResult {
    switch (st) {
        .expr => |e| {
            _ = try state.visitExpr(e);
        },
        .function => |f| {
            const func: ast.Stmt.Function = f;
            const clone: *Frame = try state.core.env_pool.create();
            clone.* = state.core.current_env;

            try state.core.values.append(
                state.core.arena.allocator(),
                .{ .func = data.Function{
                    .decl = func.decl,
                    .closure = clone,
                } },
            );
        },
        .@"if" => |i| {
            const iff: ast.Stmt.If = i;
            const cond = try state.visitExpr(iff.condition);
            if (isTruthy(cond)) return try state.visitStmt(iff.then_branch.*);
            if (iff.else_branch) |b| return try state.visitStmt(b.*);
        },
        .@"return" => |r| {
            const ret: ast.Stmt.Return = r;
            state.core.ret_val = try state.visitExpr(ret.value);
            return error.Return;
        },
        .print => |e| {
            const p: ast.Stmt.Print = e;
            const value = try state.visitExpr(p);
            std.debug.print("{}\n", .{value});
        },
        .@"var" => |vst| {
            const v: ast.Stmt.Var = vst;
            const value: data.Value = if (v.init) |i| try state.visitExpr(i) else .{ .nil = {} };
            try state.core.values.append(state.core.arena.allocator(), value);
        },
        .@"while" => |w| {
            const wh: ast.Stmt.While = w;
            while (true) {
                const cond = try state.visitExpr(wh.condition);
                if (!isTruthy(cond)) break;
                try state.visitStmt(wh.body.*);
            }
        },
        .block => |b| {
            var frame: Frame = undefined;
            state.core.pushFrame(&frame);
            state.core.current_env.enclosing = &frame;
            defer state.core.restoreFrame(frame);
            try state.executeBlock(b);
        },
        .class => |c| {
            const class_info: ast.Stmt.Class = c;
            const class_ptr: *data.Class = try state.core.class_pool.create();
            const class_value_ptr = try state.core.values.addOne(
                state.core.arena.allocator(),
            );

            const superclass: ?*data.Class = if (class_info.superclass) |sp| buildSuper: {
                const super = state.lookupVariable(sp);
                const superc: *data.Class = if (super == .class) super.class else {
                    state.core.ctx.report(sp, "Super class must be a class");
                    return error.RuntimeError;
                };

                superc.refcount += 1;
                break :buildSuper superc;
            } else null;

            var methods: std.StringHashMapUnmanaged(data.Function) = .{};
            var init_method: ?data.Function = null;
            errdefer methods.deinit(state.core.arena.allocator());
            if (class_info.methods.len > 0) {
                const class_closure = cloneFrame: {
                    const frame: *Frame = try state.core.env_pool.create();
                    state.core.pushFrame(frame);
                    state.core.current_env.enclosing = frame;
                    break :cloneFrame frame;
                };

                errdefer {
                    state.core.restoreFrame(class_closure.*);
                    state.core.env_pool.destroy(class_closure);
                }

                for (class_info.methods) |m| {
                    const method = data.Function{
                        .decl = m.decl,
                        .closure = class_closure,
                    };
                    const is_init = std.mem.eql(u8, m.name.lexeme, "init");
                    if (is_init) init_method = method;
                    try methods.put(state.core.arena.allocator(), m.name.lexeme, method);
                }
            }

            class_ptr.* = data.Class{
                .superclass = superclass,
                .methods = methods,
                .init_method = init_method,
                .name = class_info.name.lexeme,
            };

            class_value_ptr.* = .{ .class = class_ptr };
        },
    }
}

fn getClock(_: *const anyopaque, state: *Walker, _: []const ast.Expr) data.Result {
    const conversion = comptime @intToFloat(f64, std.time.ns_per_s);
    const value = @intToFloat(f64, state.core.timer.read());
    return .{ .num = value / conversion };
}
/// Execute a block without pushing a frame
pub inline fn executeBlock(state: *Walker, block: []const ast.Stmt) data.VoidResult {
    for (block) |s| {
        try state.visitStmt(s);
    }
}

pub fn visitExpr(state: *Walker, e: ast.Expr) data.Result {
    switch (e) {
        .binary => |b| return try state.visitBinary(b),
        .literal => |l| return switch (l) {
            .string => |s| .{ .string = .{ .string = s, .alloc_refcount = null } },
            .num => |n| .{ .num = n },
            .boolean => |b| .{ .boolean = b },
            .nil => .{ .nil = {} },
        },
        .unary => |u| {
            const un: ast.Expr.Unary = u;
            const right = try state.visitExpr(un.right.*);
            switch (un.operator.ty) {
                .BANG => return .{ .boolean = !isTruthy(right) },
                .MINUS => switch (right) {
                    .num => |n| return .{ .num = -n },
                    else => {
                        state.core.ctx.report(un.operator, "Operand must be a number");
                        return error.RuntimeError;
                    },
                },
                else => unreachable,
            }
        },
        .@"var" => |v| return state.lookupVariable(v),
        .lambda => |l| {
            const lambda: ast.Expr.Lambda = l;
            const frame: *Frame = try state.core.env_pool.create();
            frame.* = state.core.current_env;
            return .{ .func = data.Function{
                .decl = lambda.decl,
                .closure = frame,
            } };
        },
        .assign => |a| {
            const assign: ast.Expr.Assign = a;
            var value = try state.visitExpr(assign.value.*);
            value.addRef();

            const distance: data.Depth = state.locals.get(local(assign.name)).?;
            const ancestor = state.core.current_env.ancestor(distance.env);
            state.core.values.items[ancestor.values_begin..][distance.stack] = value;

            return value;
        },
        .call => |c| {
            const call_info: ast.Expr.Call = c;
            const callee = try state.visitExpr(call_info.callee.*);

            switch (callee) {
                .func => |*f| {
                    const func: *const data.Function = f;
                    try @call(.always_inline, Core.checkCallArgCount, .{
                        &state.core,             func.decl.params.len,
                        call_info.arguments.len, call_info.paren,
                    });
                    return try @call(.always_inline, data.Function.makeCall, .{
                        func,
                        state,
                        call_info.arguments,
                    });
                },
                .class => |cl| {
                    const cp: *const data.Class = cl;
                    const arity = if (cp.init_method) |m| m.decl.params.len else 0;
                    try @call(.always_inline, Core.checkCallArgCount, .{
                        &state.core,     arity, call_info.arguments.len,
                        call_info.paren,
                    });
                    return try @call(.always_inline, data.Class.call, .{
                        cp, state, call_info.arguments,
                    });
                },
                .callable => |v| {
                    const vt: data.CallableVT = v;
                    try @call(.always_inline, Core.checkCallArgCount, .{
                        &state.core,     vt.arity, call_info.arguments.len,
                        call_info.paren,
                    });
                    return try vt.call(vt.ptr, state, call_info.arguments);
                },
                else => {
                    state.core.ctx.report(call_info.paren, "Can only call native fns, classes or functions");
                    return error.RuntimeError;
                },
            }
        },
        .this => |t| return state.lookupVariable(t),
        .super => |s| {
            const super: ast.Expr.Super = s;
            const this = state.lookupVariable(super.keyword);
            return try state.core.superGet(this, super.method);
        },
        .get => |g| {
            const get: ast.Expr.Get = g;
            const this = try state.visitExpr(get.obj.*);
            return try state.core.instanceGet(this, get.name);
        },
        .set => |s| {
            const set: ast.Expr.Set = s;
            const this = try state.visitExpr(set.obj.*);
            const instance = try state.core.checkInstancePut(this, set.name);
            var val = try state.visitExpr(set.value.*);
            val.addRef();
            try state.core.instancePut(instance, set.name, val);
            return val;
        },
    }
}

inline fn lookupVariable(state: *Walker, v: Token) data.Value {
    const distance: data.Depth = state.locals.get(local(v)).?;
    const frame = state.core.current_env.ancestor(distance.env);
    const value: data.Value = state.core.values.items[frame.values_begin..][distance.stack];
    return value;
}

inline fn isTruthy(obj: data.Value) bool {
    return switch (obj) {
        .nil => false,
        .boolean => |b| b,
        else => true,
    };
}

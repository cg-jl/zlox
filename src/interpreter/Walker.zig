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
    core.values.appendAssumeCapacity(.{ .builtin_clock = {} });

    return Walker{ .core = core, .locals = undefined };
}

inline fn visitBinary(state: *Walker, b: ast.Expr.Binary) data.Result {
    const left = try state.visitExpr(b.left.*);
    const right = try state.visitExpr(b.right.*);

    return state.core.endBinary(left, right, b.operator);
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
            if (Core.isTruthy(cond)) return try state.visitStmt(iff.then_branch.*);
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
                if (!Core.isTruthy(cond)) break;
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

/// Execute a block without pushing a frame
pub inline fn executeBlock(state: *Walker, block: []const ast.Stmt) data.VoidResult {
    for (block) |s| {
        try state.visitStmt(s);
    }
}

pub fn visitExpr(state: *Walker, e: ast.Expr) data.Result {
    switch (e) {
        .binary => |b| return try state.visitBinary(b),
        .literal => |l| return Core.literalToValue(l),
        .unary => |u| {
            const un: ast.Expr.Unary = u;
            const right = try state.visitExpr(un.right.*);
            return state.core.endUnary(right, un.operator);
        },
        .@"var" => |v| return state.lookupVariable(v),
        .lambda => |l| {
            const lambda: ast.FuncDecl = l;
            const frame: *Frame = try state.core.env_pool.create();
            frame.* = state.core.current_env;
            return .{ .func = data.Function{
                .decl = lambda,
                .closure = frame,
            } };
        },
        .assign => |a| {
            const assign: ast.Expr.Assign = a;
            var value = try state.visitExpr(assign.value.*);
            value.addRef();

            const distance: data.Depth = state.locals.get(local(assign.name)).?;
            state.core.valueAt(distance).* = value;

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
                    return state.makeCall(func, call_info.arguments);
                },
                .class => |cl| {
                    const cp: *const data.Class = cl;
                    const arity = if (cp.init_method) |m| m.decl.params.len else 0;
                    try @call(.always_inline, Core.checkCallArgCount, .{
                        &state.core,     arity, call_info.arguments.len,
                        call_info.paren,
                    });

                    const instance: *data.Instance = try state.core.instance_pool.create();
                    errdefer state.core.instance_pool.destroy(instance);

                    if (cl.init_method) |m| {
                        const bound = try state.core.bind(m, instance);
                        defer state.core.unbind(bound);
                        try state.makeInitCall(&bound, call_info.arguments);
                    }

                    return .{ .instance = instance };
                },
                .builtin_clock => {
                    try @call(.always_inline, Core.checkCallArgCount, .{
                        &state.core,     0, call_info.arguments.len,
                        call_info.paren,
                    });
                    return state.core.getClock();
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
    return state.core.valueAt(distance).*;
}

inline fn setupCall(
    st: *Walker,
    func: *const data.Function,
    args: []const ast.Expr,
    frame: *Frame,
) data.AllocOrSignal!void {
    try st.core.values.ensureUnusedCapacity(st.core.arena.allocator(), args.len);
    for (args) |a| {
        st.core.values.appendAssumeCapacity(try st.visitExpr(a));
    }

    st.core.pushFrame(frame);

    // Make sure that the ancestor calls point to the correct environment.
    st.core.current_env.enclosing = func.closure;
    // Make sure that the arguments are popped too.
    // We don't create the frame before the arguments are evaluated
    // because then we introduce a new scope where it shouldn't be.
    st.core.current_env.values_begin -= args.len;
}

fn makeInitCall(
    st: *Walker,
    func: *const data.Function,
    args: []const ast.Expr,
) data.VoidResult {
    var frame: Frame = undefined;
    try st.setupCall(func, args, &frame);
    defer st.core.restoreFrame(frame);

    st.executeBlock(func.decl.body) catch |err| {
        if (err != error.Return) return err;
        if (st.core.ret_val) |*r| {
            r.dispose(&st.core);
        }
        st.core.ret_val = null;
    };
}
pub fn makeCall(
    st: *Walker,
    func: *const data.Function,
    args: []const ast.Expr,
) data.Result {
    var frame: Frame = undefined;

    try st.setupCall(func, args, &frame);
    defer st.core.restoreFrame(frame);

    const ret_val: data.Value = catchReturn: {
        st.executeBlock(func.decl.body) catch |err| {
            if (err == error.Return) {
                const ret = st.core.ret_val orelse data.Value.nil();
                st.core.ret_val = null;
                break :catchReturn ret;
            } else return err;
        };
        break :catchReturn data.Value.nil();
    };

    return ret_val;
}

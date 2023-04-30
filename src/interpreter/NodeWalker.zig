const NodeWalker = @This();
const Core = @import("Core.zig");
const Ast = @import("../ast/Ast.zig");
const Resolver = @import("NodeResolver.zig");
const Frame = @import("Frame.zig");
const Token = @import("../Token.zig");
const data = @import("data.zig");
const context = @import("../context.zig");
const std = @import("std");

const AllocErr = std.mem.Allocator.Error;

ast: Ast,
core: Core,
locals: Resolver.LocalMap,

pub fn tryVisitBlock(w: *NodeWalker, block: []const Ast.Index) AllocErr!void {
    for (block) |ind| {
        _ = w.visitNode(ind) catch |err| {
            std.debug.assert(err != error.Return);
            if (err == error.RuntimeError) {
                const last_error = w.core.ctx.last_error.?;
                context.reportToken(last_error.token, last_error.message);
                return;
            }
            return @errSetCast(AllocErr, err);
        };
    }
}

pub fn visitNode(w: *NodeWalker, node_index: Ast.Index) data.Result {
    const tag = w.ast.nodes.items(.tag)[node_index];
    switch (tag) {
        .binary => {
            const bin: Ast.Binary = w.ast.unpack(Ast.Binary, node_index);
            const lhs = try w.visitNode(bin.lhs);
            const rhs = try w.visitNode(bin.rhs);
            return w.core.endBinary(lhs, rhs, bin.op);
        },
        .literal => {
            const literal: Ast.Literal = w.ast.unpack(Ast.Literal, node_index);
            return Core.literalToValue(literal.extractLiteral());
        },
        .unary => {
            const unpacked = w.ast.unpack(Ast.Unary, node_index);
            const rhs = try w.visitNode(unpacked.rhs);
            return w.core.endUnary(rhs, unpacked.op);
        },
        .fetchVar => {
            const token = w.ast.unpack(Ast.FetchVar, node_index);
            const depth: data.Depth = w.locals.get(Resolver.local(token)).?;
            return w.core.valueAt(depth).*;
        },
        .assign => {
            const unpacked = w.ast.unpack(Ast.Assign, node_index);
            const depth: data.Depth = w.locals.get(Resolver.local(unpacked.name)).?;
            const valuep = w.core.valueAt(depth);
            var rhs = try w.visitNode(unpacked.rhs);
            rhs.addRef();
            valuep.* = rhs;
            return rhs;
        },
        .call => {
            const unpacked = w.ast.unpack(Ast.Call, node_index);
            const callee: data.Value = try w.visitNode(unpacked.callee);
            return switch (callee) {
                .builtin_clock => {
                    try @call(.always_inline, Core.checkCallArgCount, .{
                        &w.core, 0, unpacked.params.len(), unpacked.paren,
                    });
                    return w.core.getClock();
                },
                .class => |cl| {
                    const instance: *data.Instance = w.core.instance_pool.create();
                    errdefer w.core.instance_pool.destroy(instance);

                    if (cl.init_method) |im| {
                        try @call(.always_inline, Core.checkCallArgCount, .{
                            &w.core,               im.decl.params.len(),
                            unpacked.params.len(), unpacked.paren,
                        });
                        const bound = try w.core.bind(im, instance);
                        defer w.core.unbind(bound);
                        try w.makeInitCall(bound, unpacked.params);
                    } else {
                        try @call(.always_inline, Core.checkCallArgCount, .{
                            &w.core, 0, unpacked.params.len(), unpacked.paren,
                        });
                    }

                    return data.Value{ .instance = instance };
                },
                .func => |f| {
                    try @call(.always_inline, Core.checkCallArgCount, .{
                        &w.core,        f.decl.params.len(), unpacked.params.len(),
                        unpacked.paren,
                    });

                    return w.makeRegularCall(f, unpacked.params);
                },
            };
        },
        .this => {
            const this = w.ast.unpack(Ast.This, node_index);
            return w.lookupVariable(this);
        },
        .super => {
            const super = w.ast.unpack(Ast.Super, node_index);
            const this = w.lookupVariable(super);
            return w.core.superGet(this, super);
        },
        else => @panic("not implemented"),
    }
}

inline fn lookupVariable(w: *NodeWalker, v: Token) data.Value {
    const distance: data.Depth = w.locals.get(local(v)).?;
    return w.core.valueAt(distance).*;
}

fn executeBlock(w: *NodeWalker, block: Ast.SliceIndex) data.VoidResult {
    for (block.start..block.end) |i| {
        try w.visitNode(i);
    }
}

inline fn makeInitCall(
    w: *NodeWalker,
    func: data.Function,
    args: Ast.SliceIndex,
) data.VoidResult {
    var frame: Frame = undefined;
    try w.setupCall(func, args, &frame);
    defer w.core.restoreFrame(frame);

    w.executeBlock(func.decl.body) catch |err| {
        if (err != error.Return) return err;
        if (w.core.ret_val) |*r| {
            r.dispose(&w.core);
        }
        w.core.ret_val = null;
    };
}

fn makeRegularCall(
    w: *NodeWalker,
    func: data.Function,
    args: Ast.SliceIndex,
) data.Result {
    var frame: Frame = undefined;
    try w.setupCall(func, args, &frame);
    defer w.core.restoreFrame(frame);

    w.executeBlock(func.decl.body) catch |err| {
        if (err == error.Return) {
            return w.core.takeReturn();
        }
        return err;
    };

    return data.Value.nil();
}

inline fn setupCall(
    w: *NodeWalker,
    func: data.Function,
    args: Ast.SliceIndex,
    frame: *Frame,
) data.VoidResult {
    try w.core.values.ensureUnusedCapacity(
        w.core.arena.allocator(),
        args.end - args.start,
    );

    for (args.start..args.end) |i| {
        w.core.values.appendAssumeCapacity(try w.visitNode(i));
    }

    w.core.pushFrame(frame);
    // Make sure that the ancestor calls point to the correct environment.
    w.core.current_env.enclosing = func.closure;
    // Make sure that the arguments are popped too.
    // We don't create the frame before the arguments are evaluated
    // because then we introduce a new scope where it shouldn't be.
    w.core.current_env.values_begin -= args.end - args.start;
}

pub fn initCore(gpa: std.mem.Allocator, ast: Ast) !NodeWalker {
    var core = try Core.init(gpa, 1);
    core.values.appendAssumeCapacity(.{ .builtin_clock = {} });

    return NodeWalker{ .core = core, .ast = ast, .locals = undefined };
}

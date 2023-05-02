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
locals: Resolver.DepthMap,

pub fn tryPrintNode(w: *NodeWalker, index: Ast.Index) AllocErr!void {
    const value = try w.tryVisitNode(index);
    std.debug.print("{}\n", .{value});
}

fn tryVisitNode(w: *NodeWalker, index: Ast.Index) AllocErr!data.Value {
    return w.visitNode(index) catch |err| {
        std.debug.assert(err != error.Return);
        if (err == error.RuntimeError) {
            const last_error = w.core.ctx.last_error.?;
            context.reportSource(last_error.source, last_error.message);
            return data.Value.nil();
        }
        return @errSetCast(AllocErr, err);
    };
}

pub fn tryVisitBlock(w: *NodeWalker, block: []const Ast.Index) AllocErr!void {
    for (block) |ind| {
        _ = try w.tryVisitNode(ind);
    }
}

// statement visits return nil because I can't return undefined.
pub fn visitNode(w: *NodeWalker, node_index: Ast.Index) data.Result {
    const tag = w.ast.nodes.items(.tag)[node_index];
    switch (tag) {
        .binary => {
            const bin: Ast.Binary = w.ast.unpack(Ast.Binary, node_index);
            const lhs = try w.visitNode(bin.lhs);
            const rhs = try w.visitNode(bin.rhs);
            return w.endBinary(node_index, bin.op, lhs, rhs);
        },
        .literal => return w.ast.unpack(Ast.Literal, node_index),
        .unary => {
            const unpacked = w.ast.unpack(Ast.Unary, node_index);
            const rhs = try w.visitNode(unpacked.rhs);
            return w.endUnary(node_index, unpacked.op, rhs);
        },
        .fetchVar => {
            const token = w.ast.unpack(Ast.FetchVar, node_index);
            return w.lookupVariable(token);
        },
        .assign => {
            const unpacked = w.ast.unpack(Ast.Assign, node_index);
            const depth: data.Depth = w.locals.get(
                Resolver.local(unpacked.name),
            ).?;
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
                    try @call(
                        .always_inline,
                        checkCallArgCount,
                        .{ w, node_index, 0, unpacked.params.len() },
                    );
                    return w.core.getClock();
                },
                .class => |cl| {
                    const instance: *data.Instance = try w.core.instance_pool.create();
                    errdefer w.core.instance_pool.destroy(instance);

                    if (cl.init_method) |im| {
                        try @call(
                            .always_inline,
                            checkCallArgCount,
                            .{
                                w,
                                node_index,
                                im.decl.params.len(),
                                unpacked.params.len(),
                            },
                        );
                        const bound = try w.core.bind(im, instance);
                        defer w.core.unbind(bound);
                        try w.makeInitCall(bound, unpacked.params);
                    } else {
                        try @call(
                            .always_inline,
                            checkCallArgCount,
                            .{
                                w,
                                node_index,
                                0,
                                unpacked.params.len(),
                            },
                        );
                    }

                    return data.Value{ .instance = instance };
                },
                .func => |f| {
                    try @call(
                        .always_inline,
                        checkCallArgCount,
                        .{
                            w,
                            node_index,
                            f.decl.params.len(),
                            unpacked.params.len(),
                        },
                    );

                    return w.makeRegularCall(f, unpacked.params);
                },
                else => {
                    const source = w.ast.unpack(Token.Source, node_index);
                    w.core.ctx.report(source, "Can only call functions or classes");
                    return error.RuntimeError;
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
        .get => {
            const get = w.ast.unpack(Ast.Get, node_index);
            const this = try w.visitNode(get.obj);
            return w.core.instanceGet(this, get.name);
        },
        .set => {
            const set = w.ast.unpack(Ast.Set, node_index);
            const this = try w.visitNode(set.obj);
            const instance = try w.core.checkInstancePut(this, set.name);
            var val = try w.visitNode(set.value);
            val.addRef();
            try w.core.instancePut(instance, set.name.lexeme, val);
            return val;
        },
        .lambda => {
            const decl = w.ast.unpack(Ast.Node.FuncDecl, node_index);
            const frame: *Frame = try w.core.env_pool.create();
            frame.* = w.core.current_env;
            return .{ .func = data.Function{
                .decl = decl,
                .closure = frame,
            } };
        },
        .single_class => {
            // NOTE: in this case we don't have `errdefer` cleanups because we
            // can't have runtime errors here, just allocation errors. With
            // allocation errors we can bubble up and just free the whole
            // arena.
            const info = w.ast.unpack(Ast.SingleClass, node_index);
            const class_ptr: *data.Class = try w.core.class_pool.create();
            const class_value_ptr = try w.core.values.addOne(w.core.arena.allocator());

            class_ptr.* = try w.buildClass(
                info.methods,
                null,
                info.name.lexeme,
            );

            class_value_ptr.* = .{ .class = class_ptr };
            return data.Value.nil();
        },
        .class => {
            // NOTE: the `errdefer`s here guard against the runtime error from
            // super class not being an actual class.
            const info = w.ast.unpack(Ast.FullClass, node_index);
            const class_ptr: *data.Class = try w.core.class_pool.create();
            errdefer w.core.class_pool.destroy(class_ptr);
            const class_value_ptr = try w.core.values.addOne(w.core.arena.allocator());
            errdefer w.core.values.items.len -= 1;

            const superclass: *data.Class = buildSuper: {
                const super = w.lookupVariable(info.superclass.source);
                const superc: *data.Class = if (super == .class) super.class else {
                    w.core.ctx.report(info.superclass.source, "Super class must be a class");
                    return error.RuntimeError;
                };
                superc.refcount += 1;
                break :buildSuper superc;
            };

            errdefer superclass.refcount -= 1;

            class_ptr.* = try w.buildClass(
                info.methods,
                superclass,
                info.name.lexeme,
            );

            class_value_ptr.* = .{ .class = class_ptr };
            return data.Value.nil();
        },
        .print => {
            const print = w.ast.unpack(Ast.Print, node_index);
            const val = try w.visitNode(print);
            std.debug.print("{}\n", .{val});
            return data.Value.nil();
        },
        .ret => {
            const ret = w.ast.unpack(Ast.Return, node_index);
            w.core.ret_val = try w.visitNode(ret);
            return error.Return;
        },
        .naked_ret => return error.Return,
        .naked_var_decl => {
            try w.core.values.append(w.core.arena.allocator(), data.Value.nil());
            return data.Value.nil();
        },
        .init_var_decl => {
            const init_var_decl = w.ast.unpack(Ast.InitVarDecl, node_index);
            const init = try w.visitNode(init_var_decl.init);
            try w.core.values.append(w.core.arena.allocator(), init);
            return init;
        },
        .function => {
            const func = w.ast.unpack(Ast.Node.FuncDecl, node_index);
            const clone: *Frame = try w.core.env_pool.create();
            clone.* = w.core.current_env;

            try w.core.values.append(
                w.core.arena.allocator(),
                .{
                    .func = data.Function{
                        .decl = func,
                        .closure = clone,
                    },
                },
            );
            return data.Value.nil();
        },
        .if_simple => {
            const if_simple = w.ast.unpack(Ast.IfSimple, node_index);
            const cond = try w.visitNode(if_simple.cond);
            if (Core.isTruthy(cond)) return w.visitNode(if_simple.then_branch);
            return data.Value.nil();
        },
        .@"if" => {
            const full_if = w.ast.unpack(Ast.FullIf, node_index);
            const cond = try w.visitNode(full_if.cond);
            if (Core.isTruthy(cond)) return w.visitNode(full_if.then_branch);
            return w.visitNode(full_if.else_branch);
        },
        .naked_while => {
            const body = w.ast.unpack(Ast.NakedWhile, node_index);
            while (true) {
                _ = try w.visitNode(body);
            }
            return data.Value.nil();
        },
        .@"while" => {
            const while_data = w.ast.unpack(Ast.While, node_index);
            while (Core.isTruthy(try w.visitNode(while_data.cond))) {
                _ = try w.visitNode(while_data.body);
            }
            return data.Value.nil();
        },
        .block => {
            const block = w.ast.unpack(Ast.Block, node_index);
            var frame: Frame = undefined;
            w.core.pushFrame(&frame);
            w.core.current_env.enclosing = &frame;
            defer w.core.restoreFrame(frame);
            try w.executeBlock(block);
            return data.Value.nil();
        },
    }
}

inline fn checkCallArgCount(
    w: *NodeWalker,
    node_index: Ast.Index,
    expected: usize,
    got: usize,
) data.AllocOrSignal!void {
    if (expected != got) {
        const source = w.ast.unpack(Token.Source, node_index);
        w.core.ctx.report(source, try std.fmt.allocPrint(
            w.core.ctx.ally(),
            "Expected {} arguments but got {}",
            .{ expected, got },
        ));
        return error.RuntimeError;
    }
}

inline fn endUnary(
    w: *NodeWalker,
    node_index: Ast.Index,
    op_ty: Token.Ty,
    value: data.Value,
) data.Result {
    switch (op_ty) {
        .BANG => return .{ .boolean = !Core.isTruthy(value) },
        .MINUS => {
            if (value == .num) {
                return .{ .num = -value.num };
            }
            const source = w.ast.unpack(Token.Source, node_index);
            w.core.ctx.report(source, "Operand must be a number");
            return error.RuntimeError;
        },
        else => unreachable,
    }
}

inline fn endBinary(
    w: *NodeWalker,
    node_index: Ast.Index,
    op_ty: Token.Ty,
    left: data.Value,
    right: data.Value,
) data.Result {
    switch (op_ty) {
        .MINUS => {
            const checked = try w.checkNumberOperands(node_index, left, right);
            return .{ .num = checked.left - checked.right };
        },
        .SLASH => {
            const checked = try w.checkNumberOperands(node_index, left, right);
            return .{ .num = checked.left / checked.right };
        },
        .STAR => {
            const checked = try w.checkNumberOperands(node_index, left, right);
            return .{ .num = checked.left * checked.right };
        },
        .GREATER => {
            const checked = try w.checkNumberOperands(node_index, left, right);
            return .{ .boolean = checked.left > checked.right };
        },
        .GREATER_EQUAL => {
            const checked = try w.checkNumberOperands(node_index, left, right);
            return .{ .boolean = checked.left >= checked.right };
        },
        .LESS => {
            const checked = try w.checkNumberOperands(node_index, left, right);
            return .{ .boolean = checked.left < checked.right };
        },
        .LESS_EQUAL => {
            const checked = try w.checkNumberOperands(node_index, left, right);
            return .{ .boolean = checked.left <= checked.right };
        },
        .EQUAL_EQUAL => {
            return .{ .boolean = Core.valuesEqual(left, right) };
        },
        .BANG_EQUAL => {
            return .{ .boolean = !Core.valuesEqual(left, right) };
        },

        .PLUS => {
            if (left == .num and right == .num) {
                return .{ .num = left.num + right.num };
            } else if (left == .string and right == .string) {
                return .{ .string = .{
                    .string = try std.fmt.allocPrint(
                        w.core.ctx.ally(),
                        "{s}{s}",
                        .{ left.string.string, right.string.string },
                    ),
                    .alloc_refcount = 0,
                } };
            }
            const source = w.ast.unpack(Token.Source, node_index);
            w.core.ctx.report(source, "Operands must be two numbers or two strings");
            return error.RuntimeError;
        },

        else => unreachable,
    }
}

inline fn checkNumberOperands(
    w: *NodeWalker,
    node_index: Ast.Index,
    left: data.Value,
    right: data.Value,
) data.Signal!struct { left: f64, right: f64 } {
    if (left == .num and right == .num) {
        return .{ .left = left.num, .right = right.num };
    }
    const source = w.ast.unpack(Token.Source, node_index);
    w.core.ctx.report(source, "Both operands must be numbers");
    return error.RuntimeError;
}

const Methods = std.StringHashMapUnmanaged(data.Function);

inline fn buildClass(
    w: *NodeWalker,
    methods_slice: Ast.SliceIndex,
    superclass: ?*data.Class,
    name: []const u8,
) AllocErr!data.Class {
    var methods: Methods = .{};
    errdefer methods.deinit(w.core.arena.allocator());
    var init_method: ?data.Function = null;
    if (methods_slice.len() > 0) {
        const class_closure: *Frame = thisFrame: {
            const frame: *Frame = try w.core.env_pool.create();
            w.core.pushFrame(frame);
            w.core.current_env.enclosing = frame;
            break :thisFrame frame;
        };

        errdefer {
            w.core.restoreFrame(class_closure.*);
            w.core.env_pool.destroy(class_closure);
        }
        for (methods_slice.start..methods_slice.end) |i| {
            std.debug.assert(w.ast.nodes.items(.tag)[i] == .function);
            const func = w.ast.unpack(Ast.Function, i);
            const method = data.Function{
                .decl = func.decl,
                .closure = class_closure,
            };
            const is_init = std.mem.eql(u8, func.name.lexeme, "init");
            if (is_init) init_method = method;
            try methods.put(
                w.core.arena.allocator(),
                func.name.lexeme,
                method,
            );
        }
    }

    return data.Class{
        .methods = methods,
        .superclass = superclass,
        .init_method = init_method,
        .name = name,
    };
}

inline fn lookupVariable(w: *NodeWalker, v: Token.Source) data.Value {
    const distance: data.Depth = w.locals.get(Resolver.local(v)).?;
    return w.core.valueAt(distance).*;
}

fn executeBlock(w: *NodeWalker, block: Ast.SliceIndex) data.VoidResult {
    for (block.start..block.end) |i| {
        _ = try w.visitNode(@intCast(Ast.Index, i));
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
        w.core.values.appendAssumeCapacity(try w.visitNode(@intCast(Ast.Index, i)));
    }

    w.core.pushFrame(frame);
    // Make sure that the ancestor calls point to the correct environment.
    w.core.current_env.enclosing = func.closure;
    // Make sure that the arguments are popped too.
    // We don't create the frame before the arguments are evaluated
    // because then we introduce a new scope where it shouldn't be.
    w.core.current_env.values_begin -= args.len();
}

pub fn initCore(gpa: std.mem.Allocator, ast: Ast) !NodeWalker {
    var core = try Core.init(gpa, 1);
    core.values.appendAssumeCapacity(.{ .builtin_clock = {} });

    return NodeWalker{ .core = core, .ast = ast, .locals = undefined };
}

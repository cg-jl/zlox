const Vm = @This();
const data = @import("../data.zig");
const bytecode = @import("../bytecode.zig");
const std = @import("std");
const Frame = @import("../interpreter/Env.zig");
const Ctx = @import("../interpreter/Ctx.zig");

stack: std.ArrayList(data.Value) = .{},
class_pool: std.heap.MemoryPool(data.Class),
frame_pool: std.heap.MemoryPool(Frame),
current_frame: Frame,
ctx: Ctx,

pub fn init(gpa: std.mem.Allocator, ctx: Ctx) Vm {
    return .{
        .stack = std.ArrayList(data.Value).init(gpa),
        .class_pool = std.heap.MemoryPool(data.Class).init(gpa),
        .frame_pool = std.heap.MemoryPool(Frame).init(gpa),
        .ctx = ctx,
    };
}

pub inline fn deinit(vm: *Vm) void {
    vm.stack.deinit();
}

pub fn interpret(vm: *Vm, block: bytecode.Block) !void {
    for (block.opcodes, 0..) |o, i| {
        switch (o) {
            .local => |d| {
                const depth: data.Depth = d;
                const frame = vm.current_frame.ancestor(depth.env);
                try vm.stack.append(vm.stack.items[frame.values_begin..][depth.stack]);
            },
            .super_method => |sp| {
                const method_name: []const u8 = sp;
                const instance: *data.Instance = switch (vm.stack.pop()) {
                    .instance => |p| p,
                    else => unreachable,
                };
                const superclass: *const data.Class =
                    instance.class.superclass.?;

                const method: data.Function = superclass.findMethod(method_name) orelse {
                    vm.ctx.report(block.positions[i], try std.fmt.allocPrint(
                        vm.ctx.ally(),
                        "Undefined property '{s}'",
                        .{method_name},
                    ));
                    return error.RuntimeError;
                };
            },
            .ret => return,
            .print => std.debug.print("{}\n", .{vm.stack.pop()}),
            .constant => |c| {
                try vm.stack.append(c);
            },
            .not => {
                const value = vm.stack.pop();
                vm.stack.appendAssumeCapacity(.{ .boolean = !isTruthy(value) });
            },
            .negate => {
                const value = vm.stack.pop();
                if (value != .num) {
                    vm.ctx.report(block.positions[i], "Can only negate numbers");
                }
                vm.stack.appendAssumeCapacity(.{ .num = -value.num });
            },
            .lt => {
                const right = vm.stack.pop();
                const left = vm.stack.pop();
                const checked = try vm.checkNumberOperands(block.positions[i], left, right);
                try vm.stack.append(.{ .boolean = checked.left < checked.right });
            },
            .gt => {
                const right = vm.stack.pop();
                const left = vm.stack.pop();
                const checked = try vm.checkNumberOperands(block.positions[i], left, right);
                try vm.stack.append(.{ .boolean = checked.left > checked.right });
            },
            .mul => {
                const right = vm.stack.pop();
                const left = vm.stack.pop();
                const checked = try vm.checkNumberOperands(block.positions[i], left, right);
                try vm.stack.append(.{ .num = checked.left * checked.right });
            },
            .div => {
                const right = vm.stack.pop();
                const left = vm.stack.pop();
                const checked = try vm.checkNumberOperands(block.positions[i], left, right);
                try vm.stack.append(.{ .num = checked.left / checked.right });
            },
            .sub => {
                const right = vm.stack.pop();
                const left = vm.stack.pop();
                const checked = try vm.checkNumberOperands(block.positions[i], left, right);
                try vm.stack.append(.{ .num = checked.left - checked.right });
            },
            .add => {
                // Since we make type errors hard errors,
                // we can do lazy checks.
                const right = vm.stack.pop();
                switch (right) {
                    .num => |rn| switch (vm.stack.pop()) {
                        .num => |ln| return try vm.stack.append(.{ .num = ln + rn }),
                        else => {},
                    },
                    .string => |rs| switch (vm.stack.pop()) {
                        .string => |ls| return try vm.stack.append(.{ .string = .{
                            .string = try std.fmt.allocPrint(
                                state.ctx.ally(),
                                "{s}{s}",
                                .{ ls.string, rs.string },
                            ),
                            .was_allocated = true,
                        } }),
                    },
                    else => {},
                }
                vm.ctx.report(block.positions[i], "Operands must be two numbers or two strings");
                return error.RuntimeError;
            },
        }
    }
}
inline fn checkNumberOperands(
    vm: *Vm,
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
    vm.ctx.report(op, "Operands must be numbers");
    return error.RuntimeError;
}
fn isTruthy(obj: data.Value) bool {
    return switch (obj) {
        .nil => false,
        .boolean => |b| b,
        else => true,
    };
}

pub fn bind(vm: *Vm, f: data.Function, instancep: *data.Instance) !data.Function {
    // This frame has to be on the heap, since we're making a reference
    // out of it.
    const frame: *Frame = try vm.frame_pool.create();
    errdefer vm.frame_pool.destroy(frame);
}

pub fn pushFrame(vm: *Vm, frame: *Frame) void {
    frame.* = vm.current_frame;
    vm.current_frame = .{
        .enclosing = frame,
        .values_begin = vm.stack.items.len,
    };
}

fn disposeValues(vm: *Vm, vs: []data.Value) void {
    for (vs) |*v| {
        if (v.depcount() == 0)
            v.dispose(vm);
    }
}

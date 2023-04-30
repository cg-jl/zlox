const data = @import("data.zig");
const Token = @import("../Token.zig");

pub const Op = union(enum(u1)) {
    /// Expects the instance (this) to be in the stack.
    /// Searches for said method in the superclass and binds it to the instance.
    super_method: []const u8,
    /// Fetches the value of the local at said depth and stores it into the
    /// stack.
    local: data.Depth,
    gt: void,
    lt: void,
    negate: void,
    not: void,
    add: void,
    sub: void,
    mul: void,
    div: void,
    constant: data.Value,
    print: void,
    ret: void,
};

pub const Block = struct {
    opcodes: []const Op,
    // Store the positions in a different array; since
    // they are only accessed when erroring.
    // There will be some extra 'undefined' for the instructions:
    // - constant
    // - binary leq/geq (2 opcodes, the first one has the check and the second one
    // always passes)
    positions: []const Token,
};

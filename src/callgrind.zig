//! Callgrind utility functions used in the project.

pub extern fn startInstrumentation() callconv(.C) void;
pub extern fn stopInstrumentation() callconv(.C) void;

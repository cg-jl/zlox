const std = @import("std");
fn Tagged(comptime T: type) type {
    return struct {
        tag: []const u8,
        value: T,

        const Tag = @This();

        pub fn format(
            slf: Tag,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) @TypeOf(writer).Error!void {
            try std.fmt.formatType(slf.value, fmt, options, writer, std.options.fmt_max_depth);
            try writer.writeAll(slf.tag);
        }
    };
}

pub fn makeTag(comptime T: type, orig: T, gap: T, tags: []const []const u8) Tagged(T) {
    var value = orig;
    const tag = for (tags[0 .. tags.len - 1]) |t| {
        if (value < gap) break t;
        value /= gap;
    } else tags[tags.len - 1];
    return .{ .tag = tag, .value = value };
}

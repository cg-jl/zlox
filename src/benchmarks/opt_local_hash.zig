// zig run -Doptimize=ReleaseFast src/benchmarks/opt_local_hash.zig --enable-cache --cache-dir zig-cache
// Locals already have unique representations.
// If their packed representation uses lines as the last bits,
// we can have a "fast" hash (nothing special to do) without
// sacrificing too many collisions.

const std = @import("std");
const linux = std.os.linux;
const makeTag = @import("pretty.zig").makeTag;

var prng = std.rand.DefaultPrng.init(0);
const random = prng.random();

// We'll use a benchmark-specific `Local` struct to not interfer with the
// main codebase.
const Local = packed struct {
    col: u32,
    line: u16,
};

// NOTE: we're generating uniform locals, which is not the case.
// The locals are more akin to a normal distribution, where I don't exactly know
// avg or stddev
fn randomLocal() Local {
    return .{
        // Having max u32/u16 as the top range isn't very realistic, I'll
        // have to imitate a distribution where most of them are low values,
        // and only a few of them are high values.
        .col = random.uintAtMost(u32, std.math.maxInt(u32)),
        .line = random.uintAtMost(u16, std.math.maxInt(u16)),
    };
}
const time_tags = [_][]const u8{ "ns", "us", "ms", "s" };

const Measurement = struct {
    mean: f64,
    stddev: f64,
    min: u64,
    max: u64,

    pub fn format(
        slf: Measurement,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.writeAll("μ = ");
        try makeTag(f64, slf.mean, 1000.0, &time_tags).format(fmt, options, writer);
        try writer.writeAll("/it \t σ = ");
        try makeTag(f64, slf.stddev, 1000.0, &time_tags).format(fmt, options, writer);
        try writer.print("/it\trange: [{}, {}]", .{ slf.min, slf.max });
    }
};

// NOTE: we're integrating the 'elapsed sum' already to know if we're past
// `max_time` during the benchmarks, so why not just accept it as a parameter?
fn calculateMeasurement(elapsed: []const u64, elapsed_sum: u64) Measurement {
    const mean = @intToFloat(f64, elapsed_sum) / @intToFloat(f64, elapsed.len);
    const stddev = std.math.sqrt(sumVariance: {
        var variance: f64 = 0;
        for (elapsed) |e| {
            const diff = @intToFloat(f64, e) - mean;
            variance += diff * diff;
        }
        break :sumVariance variance / @intToFloat(f64, elapsed.len);
    });

    const minmax = std.mem.minMax(u64, elapsed);

    return Measurement{
        .mean = mean,
        .stddev = stddev,
        .min = minmax.min,
        .max = minmax.max,
    };
}

const Config = struct {
    // max time in ns, per benchmark.
    max_time: u64,
    min_iterations: u64,
    max_iterations: u64,
};

fn benchmark(
    context: anytype,
    gpa: std.mem.Allocator,
    config: Config,
) !void {
    var timer = try std.time.Timer.start();
    var locals = try std.ArrayListUnmanaged(Local).initCapacity(gpa, config.max_iterations);
    var elapsed_array = try std.ArrayListUnmanaged(u64).initCapacity(gpa, config.min_iterations);
    defer locals.deinit(gpa);
    const HashMap = std.HashMapUnmanaged(Local, usize, @TypeOf(context), std.hash_map.default_max_load_percentage);
    var hmap = HashMap{};
    defer hmap.deinit(gpa);

    {
        var i: usize = 0;
        timer.reset();
        while (i < config.min_iterations or (i < config.max_iterations and
            timer.read() < config.max_time)) : (i += 1)
        {
            const l = randomLocal();
            try hmap.put(gpa, l, i);
            try locals.append(gpa, l);
        }
    }

    // read throughput, where all the keys exist.
    // Ideal case for the `fib.lox` and `fib_opt.lox` codes, where all of the
    // values are local during the function calls.
    const read_all_in = benchAllIn: {
        var total_elapsed: u64 = 0;
        elapsed_array.clearRetainingCapacity();

        for (locals.items[0..config.min_iterations]) |l| {
            timer.reset();
            std.mem.doNotOptimizeAway(
                hmap.get(l),
            );
            const elapsed = timer.read();
            total_elapsed += elapsed;
            elapsed_array.appendAssumeCapacity(elapsed);
        }

        for (locals.items[config.min_iterations..]) |l| {
            if (total_elapsed > config.max_time) break;
            timer.reset();
            std.mem.doNotOptimizeAway(
                hmap.get(l),
            );
            const elapsed = timer.read();
            total_elapsed += elapsed;
            try elapsed_array.append(gpa, elapsed);
        }

        break :benchAllIn calculateMeasurement(elapsed_array.items, total_elapsed);
    };
    std.log.info("read all in: {d:.3}", .{read_all_in});

    // read throughput, where all the keys are random, most won't exist.
    // This benchmarks a very general case, so its performance will depend
    // on how much collisions it has.
    const read_random = benchRandom: {
        var total_elapsed: u64 = 0;
        elapsed_array.clearRetainingCapacity();
        for (0..config.min_iterations) |_| {
            const l = randomLocal();
            timer.reset();
            std.mem.doNotOptimizeAway(
                hmap.get(l),
            );
            const elapsed = timer.read();
            total_elapsed += elapsed;
            elapsed_array.appendAssumeCapacity(elapsed);
        }

        for (config.min_iterations..config.max_iterations) |_| {
            if (total_elapsed > config.max_time) break;
            const l = randomLocal();
            timer.reset();
            std.mem.doNotOptimizeAway(
                hmap.get(l),
            );
            const elapsed = timer.read();
            total_elapsed += elapsed;
            try elapsed_array.append(gpa, elapsed);
        }

        break :benchRandom calculateMeasurement(elapsed_array.items, total_elapsed);
    };
    std.log.info("read random: {d:.3}", .{read_random});

    // read throughput, where 80% of the keys are proven to exist.
    // This benchmarks a more realistic case, where a function has mostly locals
    // but also makes use of global functions for utilities the language might
    // have.
    const read_mixed = benchMixed: {
        var total_elapsed: u64 = 0;
        elapsed_array.clearRetainingCapacity();
        for (0..config.min_iterations) |i| {
            const r = random.float(f32);
            const l = if (r < 0.8) locals.items[i] else randomLocal();
            timer.reset();
            std.mem.doNotOptimizeAway(
                hmap.get(l),
            );
            const elapsed = timer.read();
            total_elapsed += elapsed;
            elapsed_array.appendAssumeCapacity(elapsed);
        }

        for (config.min_iterations..config.max_iterations) |i| {
            if (total_elapsed > config.max_time) break;

            const r = random.float(f32);
            const l = if (r < 0.8) locals.items[i % locals.items.len] else randomLocal();
            timer.reset();
            std.mem.doNotOptimizeAway(
                hmap.get(l),
            );
            const elapsed = timer.read();
            total_elapsed += elapsed;
            try elapsed_array.append(gpa, elapsed);
        }

        break :benchMixed calculateMeasurement(elapsed_array.items, total_elapsed);
    };
    std.log.info("read mixed: {d:.3}", .{read_mixed});
}

const JustPackIt = struct {
    pub const eql = std.hash_map.getAutoEqlFn(Local, @This());
    pub fn hash(_: JustPackIt, local: Local) u64 {
        const T = std.meta.Int(.unsigned, @bitSizeOf(Local));
        return @bitCast(T, local);
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    const config = Config{
        .max_iterations = 1000000,
        .min_iterations = 100000,
        .max_time = 10 * std.time.ns_per_s,
    };

    std.log.info("With auto context:", .{});
    try benchmark(
        std.hash_map.AutoContext(Local){},
        arena.allocator(),
        config,
    );

    std.log.info("Just packing it:", .{});
    try benchmark(
        JustPackIt{},
        arena.allocator(),
        config,
    );
}

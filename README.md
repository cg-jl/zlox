# zlox

A Zig port of the tree-walk interpreter from Crafting Interpreters book.

It's about 30% faster than the Java version, which took ~40s to run on my
machine.

## Runs


The benchmarks now use 100000 runs for the `fib_opt.lox` script and 100 runs
for the `fib.lox` script, taking ~45 minutes to finish. The new benchmarks now
output the mean (μ), standard deviation (σ) and median (η) of the results. The
median (η) is selected for performance comparisons, since it's the best option
given that I run the benchmarks in the same machine I develop in, which means
that there are other processes like the browser getting in the way sometimes.

The C benchmarks stay the same, since the C code is just a way to make
comparisons on how much slower is the intepreted language in regard to the C
language in debug mode.

### Reference C implementation

<details><summary>C code</summary>

```c
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int fib(int n) {
  int a = 0;
  int b = 1;
  while (n--) {
    int tmp = a;
    a += b;
    b = tmp;
  }
  return a;
}

#define NUM_SAMPLES (1ull << 25)

int main() {

  struct timespec start, end, diff;
  double avg = 0.0;
  puts("Measuring...");
  for (uint64_t i = 0; i < NUM_SAMPLES; ++i) {
    clock_gettime(CLOCK_MONOTONIC_RAW, &start);
    int x = fib(40);
    clock_gettime(CLOCK_MONOTONIC_RAW, &end);
    double sample = end.tv_nsec - start.tv_nsec;
    if (sample < 0)
      sample += 1000000000.0;
    avg += sample;
  }
  avg /= NUM_SAMPLES;

  printf("%lf ns\n", avg);

  return 0;
}
```
</details>

Compiled with `gcc -o fib fib.c`, gcc `12.2.0` and gnu libc.

<details><summary>
Output of `gcc -### -E - -march=native`
</summary>

```
Using built-in specs.
COLLECT_GCC=gcc
Target: x86_64-unknown-linux-gnu
Configured with: /builddir/gcc-12.2.0/configure --build=x86_64-unknown-linux-gnu --enable-gnu-unique-object --enable-vtable-verify --prefix=/usr --mandir=/usr/share/man --infodir=/usr/share/info --libexecdir=/usr/lib64 --libdir=/usr/lib64 --enable-threads=posix --enable-__cxa_atexit --disable-multilib --with-system-zlib --enable-shared --enable-lto --enable-plugins --enable-linker-build-id --disable-werror --disable-nls --enable-default-pie --enable-default-ssp --enable-checking=release --disable-libstdcxx-pch --with-isl --with-linker-hash-style=gnu --disable-sjlj-exceptions --disable-target-libiberty --disable-libssp --enable-languages=c,c++,objc,obj-c++,fortran,lto,go,ada
Thread model: posix
Supported LTO compression algorithms: zlib zstd
gcc version 12.2.0 (GCC)
COLLECT_GCC_OPTIONS='-E' '-march=native'
 /usr/lib64/gcc/x86_64-unknown-linux-gnu/12.2.0/cc1 -E -quiet - "-march=znver3" -mmmx -mpopcnt -msse -msse2 -msse3 -mssse3 -msse4.1 -msse4.2 -mavx -mavx2 -msse4a -mno-fma4 -mno-xop -mfma -mno-avx512f -mbmi -mbmi2 -maes -mpclmul -mno-avx512vl -mno-avx512bw -mno-avx512dq -mno-avx512cd -mno-avx512er -mno-avx512pf -mno-avx512vbmi -mno-avx512ifma -mno-avx5124vnniw -mno-avx5124fmaps -mno-avx512vpopcntdq -mno-avx512vbmi2 -mno-gfni -mvpclmulqdq -mno-avx512vnni -mno-avx512bitalg -mno-avx512bf16 -mno-avx512vp2intersect -mno-3dnow -madx -mabm -mno-cldemote -mclflushopt -mclwb -mclzero -mcx16 -mno-enqcmd -mf16c -mfsgsbase -mfxsr -mno-hle -msahf -mno-lwp -mlzcnt -mmovbe -mno-movdir64b -mno-movdiri -mmwaitx -mno-pconfig -mpku -mno-prefetchwt1 -mprfchw -mno-ptwrite -mrdpid -mrdrnd -mrdseed -mno-rtm -mno-serialize -mno-sgx -msha -mshstk -mno-tbm -mno-tsxldtrk -mvaes -mno-waitpkg -mwbnoinvd -mxsave -mxsavec -mxsaveopt -mxsaves -mno-amx-tile -mno-amx-int8 -mno-amx-bf16 -mno-uintr -mno-hreset -mno-kl -mno-widekl -mno-avxvnni -mno-avx512fp16 --param "l1-cache-size=32" --param "l1-cache-line-size=64" --param "l2-cache-size=512" "-mtune=znver3" -dumpbase -
COMPILER_PATH=/usr/lib64/gcc/x86_64-unknown-linux-gnu/12.2.0/:/usr/lib64/gcc/x86_64-unknown-linux-gnu/12.2.0/:/usr/lib64/gcc/x86_64-unknown-linux-gnu/:/usr/lib64/gcc/x86_64-unknown-linux-gnu/12.2.0/:/usr/lib64/gcc/x86_64-unknown-linux-gnu/
LIBRARY_PATH=/usr/lib64/gcc/x86_64-unknown-linux-gnu/12.2.0/:/usr/lib64/gcc/x86_64-unknown-linux-gnu/12.2.0/../../../../lib64/:/lib/../lib64/:/usr/lib/../lib64/:/usr/lib64/gcc/x86_64-unknown-linux-gnu/12.2.0/../../../:/lib/:/usr/lib/
COLLECT_GCC_OPTIONS='-E' '-march=native'
```

</details>

Program output:
```
Measuring...
44.783189 ns
```

### Recursive implementation

<details><summary>C code:</summary>

```c
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int fib(int n) {
  if (n <= 1)
    return 1;
  return fib(n - 1) + fib(n - 2);
}

#define NUM_SAMPLES 128

int main() {

  struct timespec start, end, diff;
  double avg = 0.0;
  puts("Measuring...");
  for (uint64_t i = 0; i < NUM_SAMPLES; ++i) {
    clock_gettime(CLOCK_MONOTONIC_RAW, &start);
    int x = fib(40);
    clock_gettime(CLOCK_MONOTONIC_RAW, &end);
    double sample = end.tv_nsec - start.tv_nsec;
    if (sample < 0)
      sample += 1000000000.0;
    avg += sample;
  }
  avg /= NUM_SAMPLES;

  printf("%lf ns\n", avg);

  return 0;
}
```

</details>

Program output:
```
Measuring...
421680687.820312 ns
```
~400ms.


### `fib_opt.lox` - Basic fibonacci loop

```
μ = 1.1311952400900665e-05 σ = 8.377049069412155e-07 η = 1.1171000000000002e-05
```
```
μ = 1.1205851632837888e-05 σ = 8.00675001089878e-07 η = 1.107e-05
```

Still around 11 µs (<1% improvement). The re-layout of some data did not affect
here, or it was balanced by other cache misses.


### `fib.lox` - Recursive implementation.

```
μ = 28.495360138666662 σ = 0.717394586315218 η = 28.542054694500003
```
```
μ = 27.02815113432927 σ = 0.42261051499309554 η = 27.051422258
```

About ~28~ 27s. A 5% improvement, probably because of the scale of the computation
where we switch between binary expressions and calls.

The cumulative improvement is of **80.9%**, relative to [the first working
implementation](https://github.com/cybergsus/zlox/tree/be36de134e4b64949a6483a41a0c27ff8dda5b1d)

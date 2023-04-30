# zlox

A Zig port of the tree-walk interpreter from Crafting Interpreters book.

It's about 20% faster than the Java version, which took ~40s to run on my
machine.

## Runs

These are not good benchmarks, but due to the slowness of the implementation I'm
not really able to run them more than once, without waiting till the end of the
universe.

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
3.796320159999999e-05
```

Around 37 us. Around the average


### `fib.lox` - Recursive implementation.

```
1.65580141e+08
3.650257832e+01
```
```
1.65580141e+08
3.2387978582e+01
```

About ~36~ 32s. An 11% improvement, by removing a layer of cache indirection.
The cumulative improvement is of **79%**, relative to [the first working
implementation](https://github.com/cybergsus/zlox/tree/be36de134e4b64949a6483a41a0c27ff8dda5b1d)

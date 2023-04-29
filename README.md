# zlox

A Zig port of the tree-walk interpreter from Crafting Interpreters book.

It's slower than the Java version, and leaks quite a bunch of memory due to me
being lazy and using an arena to avoid having to free stuff.

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

Compiled with `gcc -o fib fib.c`, gcc `12.2.0` and musl libc.

<details><summary>
Output of `gcc -### -`
</summary>

```
Using built-in specs.
COLLECT_GCC=gcc
COLLECT_LTO_WRAPPER=/usr/lib64/gcc/x86_64-linux-musl/12.2.0/lto-wrapper
Target: x86_64-linux-musl
Configured with: /builddir/gcc-12.2.0/configure --build=x86_64-linux-musl --disable-gnu-unique-object --disable-libsanitizer --disable-symvers libat_cv_have_ifunc=no --enable-vtable-verify --prefix=/usr --mandir=/usr/share/man --infodir=/usr/share/info --libexecdir=/usr/lib64 --libdir=/usr/lib64 --enable-threads=posix --enable-__cxa_atexit --disable-multilib --with-system-zlib --enable-shared --enable-lto --enable-plugins --enable-linker-build-id --disable-werror --disable-nls --enable-default-pie --enable-default-ssp --enable-checking=release --disable-libstdcxx-pch --with-isl --with-linker-hash-style=gnu --disable-sjlj-exceptions --disable-target-libiberty --disable-libssp --enable-languages=c,c++,objc,obj-c++,fortran,lto,go,ada
Thread model: posix
Supported LTO compression algorithms: zlib zstd
gcc version 12.2.0 (GCC)
```

</details>

Program output:
```
Measuring...
89.850265 ns
```



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
The cumulative improvement is of **79%** (relative to be36de134e4b64949a6483a41a0c27ff8dda5b1d)

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
warning: Could not find 'clock', assuming it's global
warning: Could not find 'clock', assuming it's global
1.02334155e+08
5.35776e-04
```
Around ~500us. 1000x slower than C version


- `-Doptimize=ReleaseSafe` run:
```
warning: Could not find 'clock', assuming it's global
warning: Could not find 'clock', assuming it's global
1.02334155e+08
8.4194e-05
```

Around 84 us.



### `fib.lox` - Recursive implementation.

```
warning: Could not find 'clock', assuming it's global
warning: Could not find 'clock', assuming it's global
 1.65580141e+08
1.239713567557e+03
```
This sums up to 20m 39.713 s. Painfully slow.

- `-Doptimize=ReleaseSafe` run:
```
warning: Could not find 'clock', assuming it's global
warning: Could not find 'clock', assuming it's global
1.65580141e+08
1.57722453935e+02
```

About 2m 37s. Still painfully slow, but at least I don't have to go to bed whlie
waiting for the program to finish. This demonstrates that Zig is doing a great
job at giving LLVM hints, since it can make my program ~8x faster without going
into unsafe UB.

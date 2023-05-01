#!/bin/sh

# execute this with fib.lox and then run `callgrind_control -i on` after a quick while to ensure
# the interpreter is profiled when it's in the middle of fibonacci calls.

valgrind --tool=callgrind --simulate-cache=yes --instr-atstart=no --collect-jumps=yes --skip-plt=yes --dump-instr=yes $@

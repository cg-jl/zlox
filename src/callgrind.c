#include <valgrind/callgrind.h>

void startInstrumentation() { CALLGRIND_START_INSTRUMENTATION; }

void stopInstrumentation() { CALLGRIND_STOP_INSTRUMENTATION; }

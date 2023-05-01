import subprocess
import numpy as np
import sys
from multiprocessing import Pool

usage = ('usage: bench.py [-c <count>] <.lox script>')

count = 100000

script = None
argv = sys.argv[1:]
while argv:
    match argv[0]:
        case '-c':
            argv = argv[1:]
            if not argv:
                print(usage)
                raise SystemExit(1)
            count = int(argv[0])
            argv = argv[1:]
            continue
    if script:
        print('only one script is allowed')
        print(usage)
        raise SystemExit(1)
    script = argv[0]
    argv = argv[1:]

if script is None:
    print('usage: bench.py -c <count> <.lox script>')
    raise SystemExit(1)



def bench(_):
    _, stderr = subprocess.Popen(['./zig-out/bin/zlox', script],
                                 stderr=subprocess.PIPE).communicate()
    return float(stderr.split()[1])

with Pool() as p:
    all_values = np.fromiter(p.imap_unordered(bench, range(count)), dtype=np.float64)
    avg = np.sum(all_values) / count
    variance = np.sum(np.square(all_values - avg)) / count

print(f"μ = {avg} σ² = {variance}")

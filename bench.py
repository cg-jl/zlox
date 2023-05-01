import subprocess
import numpy as np
import sys
from multiprocessing import Pool
from tqdm import tqdm

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

def reject_outliers(data, m=2.):
    d = np.abs(data - np.median(data))
    mdev = np.median(d)
    s = d/mdev if mdev else np.zero(len(d))
    return data[s < m]

with Pool(1) as p:
    print(f'[info] Running {script} {count} times and collecting results...')
    x = np.fromiter(tqdm(p.imap_unordered(bench, range(count)),total=count), dtype=np.float64)
    print('[info] Run ended, computing statistics')
    x = reject_outliers(x)
    avg = np.mean(x)
    stddev = np.std(x)
    median = np.median(x)

print(f"μ = {avg} σ = {stddev} η = {median}")

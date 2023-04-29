import subprocess
import numpy as np
from multiprocessing import Pool


time = 0
count = 100000


def bench(_):
    _, stderr = subprocess.Popen(['./zig-out/bin/zlox', 'fib_opt.lox'],
                                 stderr=subprocess.PIPE).communicate()
    return float(stderr.split()[1])

with Pool() as p:
    all_values = np.fromiter(p.imap_unordered(bench, range(count)), dtype=np.float64)
    avg = np.sum(all_values) / count
    variance = np.sum(np.square(all_values - avg)) / count

print(f"μ = {avg} σ² = {variance}")

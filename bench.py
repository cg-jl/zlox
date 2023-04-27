import subprocess
from multiprocessing import Pool


time = 0
count = 10000


def bench(_):
    _, stderr = subprocess.Popen(['./zig-out/bin/zlox', 'fib_opt.lox'],
                                 stderr=subprocess.PIPE).communicate()
    return float(stderr.split()[1])

with Pool() as p:
    total = sum(p.imap_unordered(bench, range(count)))

print(total / count)

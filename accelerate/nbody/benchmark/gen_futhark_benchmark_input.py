#!/usr/bin/env python3

'''
Generate random benchmark input of a specified size.

Use: ./gen_futhark_benchmark_input.py <n_steps> <n_bodies> <time_step> <epsilon>
'''

import sys
import random

try:
    n_steps, n_bodies, time_step, epsilon = sys.argv[1:]
except ValueError:
    sys.exit(1)

n_bodies = int(n_bodies)
    
xps = [random.randrange(0, 1000) for i in range(n_bodies)]
yps = [random.randrange(0, 1000) for i in range(n_bodies)]
zps = [random.randrange(0, 1000) for i in range(n_bodies)]
ms = [random.randrange(30, 50) for i in range(n_bodies)]
xvs = [random.random() for i in range(n_bodies)]
yvs = [random.random() for i in range(n_bodies)]
zvs = [random.random() for i in range(n_bodies)]
xas = [0.0 for i in range(n_bodies)]
yas = [0.0 for i in range(n_bodies)]
zas = [0.0 for i in range(n_bodies)]

print(n_steps)
print(epsilon)
print(time_step)
print(xps)
print(yps)
print(zps)
print(ms)
print(xvs)
print(yvs)
print(zvs)
print(xas)
print(yas)
print(zas)

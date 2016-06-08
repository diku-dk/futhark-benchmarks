#!/usr/bin/env python3

import sys
import random

random.seed(1992) # Gives reproducible datasets (to some extent).

n = int(sys.argv[1])
try:
    desc = sys.argv[2]
except IndexError:
    desc = str(n)

rands = [random.randint(0, 2**32 - 1) for i in range(n)]

with open('radix_sort_{}.in'.format(desc), 'w') as f:
    print(rands, file=f)

rands.sort()

with open('radix_sort_{}.out'.format(desc), 'w') as f:
    print(rands, file=f)

#!/usr/bin/env python3
#
# Generate the datasets used in `write.fut` for benchmarking.

import random

def rand_list(n, n_min=-2**31, n_max=2**31 - 1):
    return [random.randint(n_min, n_max) for i in range(n)]

def gen_indices_iota(n):
    indices = list(range(n))
    values = rand_list(n)
    array = rand_list(n)
    with open('indices_iota_{}.in'.format(n), 'w') as f:
        print(indices, file=f)
        print(values, file=f)
        print(array, file=f)
    with open('indices_iota_{}.out'.format(n), 'w') as f:
        print(values, file=f)

def gen_indices_shuffled(n):
    indices = list(range(n))
    random.shuffle(indices)
    values = rand_list(n)
    array = rand_list(n)
    with open('indices_shuffled_{}.in'.format(n), 'w') as f:
        print(indices, file=f)
        print(values, file=f)
        print(array, file=f)
    for i in range(n):
        array[indices[i]] = values[i]
    with open('indices_shuffled_{}.out'.format(n), 'w') as f:
        print(array, file=f)

gen_indices_iota(10000)
gen_indices_iota(1000000)
gen_indices_shuffled(10000)
gen_indices_shuffled(1000000)

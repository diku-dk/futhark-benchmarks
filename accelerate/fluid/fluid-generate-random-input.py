#!/usr/bin/env python3

import sys
import random
import numpy as np


def print_list_flat(g, xs):
    for i in range(g):
        for j in range(g):
            print(xs[i, j], end=' ')
    print('')

def generate_input(n_steps=60, grid_resolution=128, time_step=0.1,
                   diffusion_rate=0.0001, viscosity=0.00001, initials=None,
                   backend_format='futhark'):
    g = grid_resolution + 2
    size = g * g
    
    if initials is not None:
        U0, V0, D0 = initials
    else:
        # Random forces and densities.
        U0 = np.zeros((g, g))
        V0 = np.zeros((g, g))
        D0 = np.zeros((g, g))

        for k in range(size // 600):
            i0 = random.randrange(1, g - 1)
            j0 = random.randrange(1, g - 1)
            for i in range(i0, i0 + random.randint(2, 4)):
                for j in range(j0, j0 + random.randint(2, 4)):
                    if i >= g - 1 or j >= g - 1:
                        continue
                    D0[i, j] = 100.0 * random.random()

        for i in range(size // 800):
            i = random.randrange(1, g - 1)
            j = random.randrange(1, g - 1)
            V0[i, j] = 500.0 * random.random()
            U0[i, j] = 500.0 * random.random()

    if backend_format == 'futhark':
        print(U0.tolist())
        print(V0.tolist())
        print(D0.tolist())
        print(n_steps)
        print(time_step)
        print(diffusion_rate)
        print(viscosity)
    elif backend_format == 'c':
        print(grid_resolution)
        print_list_flat(g, U0)
        print_list_flat(g, V0)
        print_list_flat(g, D0)
        print(n_steps)
        print(time_step)
        print(diffusion_rate)
        print(viscosity)

def main(args):
    try:
        n_steps = args[0]
    except IndexError:
        print('error: the number of steps must be given as the first argument',
              file=sys.stderr)
        return 1
    n_steps = int(n_steps)

    try:
        grid_resolution = args[1]
    except IndexError:
        print('error: the grid resolution must be given as the second argument',
              file=sys.stderr)
        return 1
    grid_resolution = int(grid_resolution)

    try:
        backend_format = args[2]
    except IndexError:
        print('error: the backend format ("futhark" or "c") must be given as the third argument',
              file=sys.stderr)
        return 1
    if backend_format not in ['futhark', 'c']:
        print('error: the backend format must be "futhark" or "c"',
              file=sys.stderr)
        return 1
    
    try:
        seed = args[3]
    except IndexError:
        print('error: the seed (number or "none") must be given as the fourth argument',
              file=sys.stderr)
        return 1

    if seed != "none":
        random.seed(int(seed))
    
    generate_input(n_steps=n_steps,
                   grid_resolution=grid_resolution,
                   backend_format=backend_format)
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))

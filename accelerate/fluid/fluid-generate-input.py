#!/usr/bin/env python3

import sys
import numpy as np
import random


def n_elems_expected(grid_resolution):
    return (grid_resolution + 2) * (grid_resolution + 2)

def index(i, j, grid_resolution):
    return i + (grid_resolution + 2) * j

def generate_input(n_steps=60, grid_resolution=64, time_step=0.1,
                   diffusion_rate=0.00, viscosity=0.00,
                   initials=None):
    
    size = n_elems_expected(grid_resolution)

    if initials is not None:
        U0, V0, D0 = initials
    else:
        # Random forces and densities.
        U0 = np.zeros((size,))
        V0 = np.zeros((size,))
        D0 = np.zeros((size,))

        for k in range(3):
            i0 = random.randrange(0, grid_resolution)
            j0 = random.randrange(0, grid_resolution)
            for i in range(i0, i0 + 7):
                for j in range(j0, j0 + 7):
                    t = index(i, j, grid_resolution)
                    if t >= size:
                        continue
                    D0[t] = 100.0 * random.random()

        for i in range(9):
            i = random.randrange(0, size) 
            V0[i] = 100.0 * random.random()
            U0[i] = 100.0 * random.random()

    print(U0.tolist())
    print(V0.tolist())
    print(D0.tolist())
    print(n_steps)
    print(grid_resolution)
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
    
    generate_input(n_steps=n_steps)
    return 0
    

if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))

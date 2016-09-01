# Fluid simulation

A port of Accelerate's version:
https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/fluid

Original paper ("Real-Time Fluid Dynamics for Games"):
http://www.dgp.toronto.edu/people/stam/reality/Research/pdf/GDC03.pdf

`make run` to run the GUI.  Draw with the right mouse button pressed
to add particles.  Move the cursor with the left mouse button pressed
to add forces.  Press C to clear all particles and forces.  Use Q/A
and W/S to modify viscosity and diffusion of the simulated fluid.

Note that the simulation is fairly heavyweight, so while you can
increase the size if you have a beefy GPU, you probably cannot run at
high resolutions at acceptable frame rates.

## Other versions

  + Accelerate's version:
    https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/fluid/src-acc
  + Gloss' Repa version:
    https://github.com/benl23x5/gloss/tree/master/gloss-examples/raster/Fluid/src-repa
  + Accelerate's C version:
    https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/fluid/src-c
    -- apparently from the original paper
  + Gloss' alternative C version:
    https://github.com/benl23x5/gloss/tree/master/gloss-examples/raster/Fluid/src-c
    -- clearly based on the one in Accelerate's repository
  + Our C version: the `src-c` directory -- based on the one in Accelerate's
    repository, but with focus on batch processing instead of interactivity
    (like Gloss' C version, but less extreme and hopefully easier to follow
    wrt. the related Futhark version)


## The futhark code

Please read `src-futhark/fluid.fut` -- this is the library.  It describes the
major choices.


## Program structure

The benchmark consists of an outer sequential loop which steps through the
simulation.  However, Accelerate only ever benchmarks a single step, so the
measurements below always run just one iteration of this loop.

The loop body consists of a number of maps with inner sequential loops.  These
sequential loops make up a linear solver, whose body is a map.

Apart from the common `map-loop` structure, the other nontrivial part of the
fluid benchmark is that several parts of its step function acts differently
based on whether an element lies on a one-width border around the grid, or it
lies inside the grid.  When the benchmark is run with a grid size of `N`, the full
size of the grid is actually `(N + 2) * (N + 2)` to accomodate a border around
the inner grid.  To handle the edge conditions, the step function parts in
question have an `if-then-else` body.


## Measurements

The `benchmark` directory contains a script for timing the C version, the
Futhark version, and Accelerate's version, each on pseudo-random data of a
specific size, just to get a gist of the speed relations.

## Results

**Run on napoleon.**

### Raw Futhark results

```
dataset fluid-n_steps=1-n_solver_steps=40-grid_res=100.input: 11605.20us (average; relative standard deviation: 0.06)
dataset fluid-n_steps=1-n_solver_steps=40-grid_res=1000.input: 54392.30us (average; relative standard deviation: 0.00)
dataset fluid-n_steps=1-n_solver_steps=20-grid_res=2000.input: 105529.20us (average; relative standard deviation: 0.00)
dataset fluid-n_steps=1-n_solver_steps=20-grid_res=3000.input: 227403.50us (average; relative standard deviation: 0.01)
```


### Raw Accelerate results

```
Running benchmark with n_solver_steps=40 and grid_resolution=100.
time                 53.48 ms   (51.35 ms .. 56.05 ms)
                     0.992 R²   (0.982 R² .. 0.999 R²)
mean                 53.74 ms   (52.55 ms .. 54.98 ms)
std dev              2.220 ms   (1.584 ms .. 3.105 ms)
variance introduced by outliers: 14% (moderately inflated)

Running benchmark with n_solver_steps=40 and grid_resolution=1000.
time                 194.6 ms   (189.9 ms .. 200.5 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 200.8 ms   (197.7 ms .. 204.7 ms)
std dev              4.623 ms   (2.015 ms .. 6.160 ms)
variance introduced by outliers: 14% (moderately inflated)

Running benchmark with n_solver_steps=20 and grid_resolution=2000.
time                 399.7 ms   (389.2 ms .. 411.0 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 398.6 ms   (396.3 ms .. 400.2 ms)
std dev              2.468 ms   (0.0 s .. 2.825 ms)
variance introduced by outliers: 19% (moderately inflated)

Running benchmark with n_solver_steps=20 and grid_resolution=3000.
time                 895.7 ms   (884.6 ms .. 920.7 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 887.6 ms   (884.6 ms .. 890.6 ms)
std dev              5.203 ms   (0.0 s .. 5.208 ms)
variance introduced by outliers: 19% (moderately inflated)
```

### Conclusion

Average runtimes:

| Number of steps for the linear solver | Grid resolution | Futhark | Accelerate |
| --- | --- | --- | --- |
| 40 |  100 |  11605.20us |  53480.00us |
| 40 | 1000 |  54392.30us | 194600.00us |
| 20 | 2000 | 105529.20us | 399700.00us |
| 20 | 3000 | 227403.50us | 895700.00us |


## Runtime results on gpu01-diku-apl (GTX 780 Ti)

  resolution=3000, n_steps=1, n_solver_steps=20:
     Accelerate:      154.5ms
     Futhark:         107.9ms
     Futhark speedup:   1.43x


## GUI

Go to the `gui` directory.  Run `make` and then `./fluid-gui.py
GRID_RESOLUTION`, e.g. `./fluid-gui.py 256`.

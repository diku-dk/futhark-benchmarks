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

Please read `fluid.fut` -- this is the library.  It describes the
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

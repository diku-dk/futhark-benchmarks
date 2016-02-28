# Fluid simulation

A port of Accelerate's version:
https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/fluid

Original paper ("Real-Time Fluid Dynamics for Games"):
http://www.dgp.toronto.edu/people/stam/reality/Research/pdf/GDC03.pdf


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

Please read `src-futhark/fluid.fut.module` -- this is the library.  It describes
the major choices.


## Measurements

The `benchmark` directory contains a script for timing the C version, the
Futhark version, and Accelerate's version, each on pseudo-random data of a
specific size, just to get a gist of the speed relations.


## Visualizations

Go to the `visualize` directory.

Run `./fluid-make-images.sh <n steps> <grid resolution> <backend> <output
directory> <seed>` to make `<n steps>` images of size `<grid resolution> × <grid
resolution>` with the `<backend>` `futhark` or `c`, and with the random seed
`<seed>` as a predefined number or as `none`.

After that, you can run `./fluid-images-to-video.sh <output directory>` to make
a video with the filename `<output directory>.webm`.

Python 3 dependencies:

  + numpy
  + pypng

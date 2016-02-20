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


## Measurements

TODO: Make a non-interactive version of Accelerate's code, so that a basic speed
comparison is possible.


## Visuals

Run `./fluid-make-images.sh <n steps> <grid resolution> <output directory>` to
make `<n steps>` images of size `<grid resolution> × <grid resolution>`.

After that, you can run `./fluid-images-to-video.sh <output directory>` to make
a video with the filename `<output directory>.webm`.

Python 3 dependencies:

  + numpy
  + pypng

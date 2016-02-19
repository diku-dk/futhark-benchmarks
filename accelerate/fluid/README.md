# Fluid simulation

A port of Accelerate's version:
https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/fluid


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

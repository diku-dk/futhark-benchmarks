# Mandelbrot

Port of Accelerate's
[mandelbrot](https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/mandelbrot)
example, along with a [Lys](https://github.com/diku-dk/lys/) frontend
that uses SDL to implement a interactive visualisation.  Just run

    make run

for pretty pictures.  Note that the implementation uses 32-bit
floating point numbers by default, so it can't zoom far before it gets
choppy.

## "Program uses double-precision floats, but this is not supported on the chosen device"

Double-precision support is unconditionally compiled in, but Intel
GPUs do not support these.  If you also have a non-Intel GPU, you can
try picking it with the `-d option` to the `mandelbrot-gui` binary,
e.g. `./mandelbrot-gui -dAMD' on a machine that also has an AMD
device.

Alternatively, go to `mandelbrot-gui.fut` and replace the line

    module mandelbrot64 = import "mandelbrot64"

with

    module mandelbrot64 = import "mandelbrot32"

This will make the 32-bit and 64-bit modes identical.

## Controls

The key controls are very similar to the ones in the Accelerate implementation:

| Key | Action |
| --- | ------ |
| Left mouse button | Drag the view port |
| q/e | Decrease and increase the iteration limit by a single step. |
| w/s | Zoom in and out. |
| Scroll wheel | Soom in and out. |
| z/c | Decrease and increase the radius for determining escape trajectories. |
| 0-9 | Go to a predefined part of the Mandelbrot set.  Some are invisible when using single precision. |
| p | Switch between using double- and single-precision floats. |
| Escape | Exit |

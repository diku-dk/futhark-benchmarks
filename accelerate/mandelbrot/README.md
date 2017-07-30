# Mandelbrot

Port of Accelerate's
[mandelbrot](https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/mandelbrot)
example, along with a Python frontend that uses Pygame to implement a
interactive visualisation.  Just run

    make run

for pretty pictures.  Note that the implementation uses 32-bit
floating point numbers by default, so it can't zoom far before it gets
choppy.

The key controls are very similar to the ones in the Accelerate implementation:

| Key | Action |
| --- | ------ |
| Arrow keys | Pan the view port |
| Left mouse button | Drag the view port |
| q/e | Decrease and increase the iteration limit gradually. |
| a/d | Decrease and increase the iteration by a single largish step. |
| w/s | Zoom in and out. |
| Scroll wheel | Soom in and out. |
| z/c | Decrease and increase the radius for determining escape trajectories. |
| 0-9 | Go to a predefined part of the Mandelbrot set.  Some are invisible when using single precision. |
| p | Switch between using double- and single-precision floats.  Some GPUs do not support double precision. |
| Escape | Exit |

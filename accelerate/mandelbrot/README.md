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
| z/c | Decrease and increase the radius for determining escape trajectories. |
| 0-9 | Go to a predefined part of the Mandelbrot set.  Some are invisible when using single precision. |
| p | Switch between using double- and single-precision floats.  Some GPUs do not support double precision. |
| Escape | Exit |

## Runtime results on gpu01-diku-apl (GTX 780 Ti)

    width 800, height 600:
      Accelerate:  1.289ms
      Futhark:     0.453ms
      Speedup:       2.84x

    width 1000, height 1000:
      Accelerate:  2.345ms
      Futhark:     0.664ms
      Speedup:       3.53x

    width 2000, height 2000:
      Accelerate:  8.006ms
      Futhark:     1.835ms
      Speedup:       4.36x

    width 4000, height 4000:
      Accelerate: 29.26ms
      Futhark:    6.426ms
      Speedup:      4.55x

    width 8000, height 8000:
      Accelerate:  142.000ms
      Futhark:      24.174ms
      Speedup:      5.87x

All with an iteration limit of 255.  Try it yourself:

    futhark-bench --compiler=futhark-opencl mandelbrot.fut

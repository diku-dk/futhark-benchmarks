# Mandelbrot

Port of Accelerate's
[mandelbrot](https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/mandelbrot)
example, along with a Python frontend that uses Pygame to implement a
interactive visualisation.  Just run

    make run

for pretty pictures.  Note that the implementation uses 32-bit
floating point numbers, so it can't zoom far before it gets choppy.

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

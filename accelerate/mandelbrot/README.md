Mandelbrot
==========

Port of Accelerate's
[mandelbrot](https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/mandelbrot)
example, along with a Python wrapper for easily turning the raw data
into images.  For example:

    futhark-pyopencl --module mandelbrot.fut -o mandelbrot.py
    ./mandelbrot-run.py --width 1024 --height 768 mandelbrot.png

Performance
-----------

Performance of `futhark-opencl` compared with Accelerate is as
follows, on `napoleon`:

    width 800, height 600:
      Accelerate:  2.979ms
      Futhark:     0.996ms
      Speedup:        2.9x

    width 1000, height 1000:
      Accelerate:  4.635ms
      Futhark:     1.467ms
      Speedup:        3.1x

    width 2000, height 2000:
      Accelerate: 11.080ms
      Futhark:     4.017ms
      Speedup:       2.75x

    width 4000, height 4000:
      Accelerate: 37.130ms
      Futhark:    13.679ms
      Speedup:       2.71x

    width 8000, height 8000:
      Accelerate:  140.6ms
      Futhark:      51.8ms
      Speedup:       2.71x

All with an iteration limit of 255.  Try it yourself:

    futhark-bench --compiler=futhark-opencl mandelbrot.fut

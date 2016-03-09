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

    width 1000, height 1000:
      Accelerate:  4.635ms
      Futhark:     1.467ms

    width 2000, height 2000:
      Accelerate: 11.080ms
      Futhark:     4.017ms

    width 4000, height 4000:
      Accelerate: 37.130ms
      Futhark:    13.679ms

All with an iteration limit of 255.

== Mandelbrot Explorer

Just run `make` to build, then `make run`.  Requires the Futhark
compiler, Pygame and PyOpenCL.  And of course, a working OpenCL setup.

You can move around the fractal using the arrow keys.  Use `q` and `w`
to decrease or increase the iteration limit on the computation loop.
Use `+` and `-` to zoom, or left- and right-click the mouse.

Use the Space key to toggle between using Futhark and Numpy to render
the fractal.  The latter is very slow.

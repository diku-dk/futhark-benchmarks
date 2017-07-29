-- Mandelbrot visualisation with 64-bit floats.

import "/futlib/math"
import "generic_mandelbrot"

module mandelbrot = mandelbrot f64

entry render_mandelbrot (screenX: i32) (screenY: i32)
                        (depth: i32) (radius: f64)
                        (xmin: f64) (ymin: f64) (xmax: f64) (ymax: f64)
                        : [screenX][screenY]i32 =
  mandelbrot.render_mandelbrot screenX screenY depth radius xmin ymin xmax ymax

-- Mandelbrot visualisation with 64-bit floats.

import "generic_mandelbrot"

module mandelbrot = mandelbrot f64

entry render_mandelbrot (screenX: i32) (screenY: i32)
                        (xcentre: f64) (ycentre: f64) (width: f64)
                        (limit: i32) (radius: f64)
                        : [screenY][screenX]i32 =
  mandelbrot.render_mandelbrot screenX screenY xcentre ycentre width limit radius

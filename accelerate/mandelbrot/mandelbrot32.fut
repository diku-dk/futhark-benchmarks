-- Mandelbrot visualisation with 32-bit floats.

import "generic_mandelbrot"

module mandelbrot = mandelbrot f32

entry render_mandelbrot (screenX: i32) (screenY: i32)
                        (xcentre: f32) (ycentre: f32) (width: f32)
                        (limit: i32) (radius: f32)
                        : [screenY][screenX]i32 =
  mandelbrot.render_mandelbrot screenX screenY xcentre ycentre width limit radius

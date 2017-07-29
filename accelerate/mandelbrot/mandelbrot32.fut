-- Mandelbrot visualisation with 32-bit floats.

import "generic_mandelbrot"

module mandelbrot = mandelbrot f32

entry render_mandelbrot (screenX: i32) (screenY: i32)
                        (depth: i32) (radius: f32)
                        (xmin: f32) (ymin: f32) (xmax: f32) (ymax: f32)
                        : [screenX][screenY]i32 =
  mandelbrot.render_mandelbrot screenX screenY depth radius xmin ymin xmax ymax

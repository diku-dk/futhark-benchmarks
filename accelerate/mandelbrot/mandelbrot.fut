-- Port of Accelerate's Mandelbrot example.  This can be run with futhark-bench.
--
-- ==
-- tags { futhark-c futhark-opencl }
-- input {  800  600 255 -2.23f32 -1.15f32 0.83f32 1.15f32 }
-- input { 1000 1000 255 -2.23f32 -1.15f32 0.83f32 1.15f32 }
-- input { 2000 2000 255 -2.23f32 -1.15f32 0.83f32 1.15f32 }
-- input { 4000 4000 255 -2.23f32 -1.15f32 0.83f32 1.15f32 }
-- input { 8000 8000 255 -2.23f32 -1.15f32 0.83f32 1.15f32 }

import "/futlib/math"
import "generic_mandelbrot"

module mandelbrot = mandelbrot f32

let main(screenX: i32, screenY: i32, depth: i32, xmin: f32, ymin: f32, xmax: f32, ymax: f32): [screenX][screenY]i32 =
  mandelbrot.render_mandelbrot screenX screenY depth 4f32 xmin ymin xmax ymax

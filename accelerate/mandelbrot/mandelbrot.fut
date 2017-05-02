-- Port of Accelerate's Mandelbrot example.
--
-- We use the complex number library from futlib.
--
-- ==
-- tags { futhark-c futhark-opencl }
-- input {  800  600 255 -2.23f32 -1.15f32 0.83f32 1.15f32 }
-- input { 1000 1000 255 -2.23f32 -1.15f32 0.83f32 1.15f32 }
-- input { 2000 2000 255 -2.23f32 -1.15f32 0.83f32 1.15f32 }
-- input { 4000 4000 255 -2.23f32 -1.15f32 0.83f32 1.15f32 }
-- input { 8000 8000 255 -2.23f32 -1.15f32 0.83f32 1.15f32 }

default(f32)

import "/futlib/math"
import "/futlib/complex"

module c32 = complex f32
type c32 = c32.complex

let dot(c: c32): f32 =
  let (r, i) = (c32.re c, c32.im c)
  in r * r + i * i

let divergence(depth: i32, c0: c32): i32 =
  loop ((c, i) = (c0, 0)) = while i < depth && dot(c) < 4.0 do
    (c0 c32.+ c c32.* c,
     i + 1)
  in i

let mandelbrot(screenX: i32, screenY: i32, depth: i32, view: (f32,f32,f32,f32)): [screenX][screenY]i32 =
  let (xmin, ymin, xmax, ymax) = view
  let sizex = xmax - xmin
  let sizey = ymax - ymin
  in map (\(x: i32): [screenY]i32  ->
           map (\(y: i32): i32  ->
                  let c0 = c32.mk (xmin + (f32(x) * sizex) / f32(screenX))
                                  (ymin + (f32(y) * sizey) / f32(screenY))
                  in divergence(depth, c0))
               (iota screenY))
         (iota screenX)

-- Returns RGB (no alpha channel).
let escapeToColour(depth: i32) (divergence: i32): i32 =
  if depth == divergence
  then 0
  else
    let r = 3 * divergence
    let g = 5 * divergence
    let b = 7 * divergence
    in (r<<16 | g<<8 | b)

let main(screenX: i32, screenY: i32, depth: i32, xmin: f32, ymin: f32, xmax: f32, ymax: f32): [screenX][screenY]i32 =
  let escapes = mandelbrot(screenX, screenY, depth, (xmin, ymin, xmax, ymax))
  in map (\(row: []i32): [screenY]i32  ->
            map (escapeToColour depth) row) escapes

-- Port of Accelerate's Mandelbrot example.
--
-- Complicated a little bit by the fact that Futhark does not natively
-- support complex numbers.  We will represent a complex number as a
-- tuple {f32,f32}.
--
-- ==
-- tags { futhark-c futhark-opencl }
-- input {  800  600 255 -2.23f32 -1.15f32 0.83f32 1.15f32 }
-- input { 1000 1000 255 -2.23f32 -1.15f32 0.83f32 1.15f32 }
-- input { 2000 2000 255 -2.23f32 -1.15f32 0.83f32 1.15f32 }
-- input { 4000 4000 255 -2.23f32 -1.15f32 0.83f32 1.15f32 }
-- input { 8000 8000 255 -2.23f32 -1.15f32 0.83f32 1.15f32 }

default(f32)

type complex = (f32, f32)

fun dot(c: complex): f32 =
  let (r, i) = c
  in r * r + i * i

fun multComplex(x: complex, y: complex): complex =
  let (a, b) = x
  let (c, d) = y
  in (a*c - b * d,
      a*d + b * c)

fun addComplex(x: complex, y: complex): complex =
  let (a, b) = x
  let (c, d) = y
  in (a + c,
      b + d)

fun divergence(depth: i32, c0: complex): i32 =
  loop ((c, i) = (c0, 0)) = while i < depth && dot(c) < 4.0 do
    (addComplex(c0, multComplex(c, c)),
     i + 1)
  in i

fun mandelbrot(screenX: i32, screenY: i32, depth: i32, view: (f32,f32,f32,f32)): [screenX][screenY]i32 =
  let (xmin, ymin, xmax, ymax) = view
  let sizex = xmax - xmin
  let sizey = ymax - ymin
  in map (\(x: i32): [screenY]i32  ->
           map  (\(y: i32): i32  ->
                  let c0 = (xmin + (f32(x) * sizex) / f32(screenX),
                            ymin + (f32(y) * sizey) / f32(screenY))
                  in divergence(depth, c0)
            ) (iota(screenY))) (
        iota(screenX))

fun main(screenX: i32, screenY: i32, depth: i32, xmin: f32, ymin: f32, xmax: f32, ymax: f32): [screenX][screenY]i32 =
  let escapes = mandelbrot(screenX, screenY, depth, (xmin, ymin, xmax, ymax))
  in map (\(row: []i32): [screenY]i32  ->
            map (escapeToColour depth) row) escapes

-- Returns RGB (no alpha channel).
fun escapeToColour(depth: i32) (divergence: i32): i32 =
  if depth == divergence
  then 0
  else
    let r = 3 * divergence
    let g = 5 * divergence
    let b = 7 * divergence
    in (r<<16 | g<<8 | b)

-- Port of Accelerate's Mandelbrot example.
--
-- Complicated a little bit by the fact that Futhark does not natively
-- support complex numbers.  We will represent a complex number as a
-- tuple {f32,f32}.
--
-- ==
-- tags { futhark-c futhark-opencl }
-- notravis input {  800  600 255 -2.23f32 -1.15f32 0.83f32 1.15f32 }
-- notravis input { 1000 1000 255 -2.23f32 -1.15f32 0.83f32 1.15f32 }
-- notravis input { 2000 2000 255 -2.23f32 -1.15f32 0.83f32 1.15f32 }
-- notravis input { 4000 4000 255 -2.23f32 -1.15f32 0.83f32 1.15f32 }
-- notravis input { 8000 8000 255 -2.23f32 -1.15f32 0.83f32 1.15f32 }

default(f32)

type complex = (f32, f32)

fun f32 dot(complex c) =
  let (r, i) = c in
  r * r + i * i

fun complex multComplex(complex x, complex y) =
  let (a, b) = x in
  let (c, d) = y in
  (a*c - b * d,
   a*d + b * c)

fun complex addComplex(complex x, complex y) =
  let (a, b) = x in
  let (c, d) = y in
  (a + c,
   b + d)

fun int divergence(int depth, complex c0) =
  loop ((c, i) = (c0, 0)) = while i < depth && dot(c) < 4.0 do
    (addComplex(c0, multComplex(c, c)),
     i + 1) in
  i

fun [[int,screenX],screenY] mandelbrot(int screenX, int screenY, int depth, (f32,f32,f32,f32) view) =
  let (xmin, ymin, xmax, ymax) = view in
  let sizex = xmax - xmin in
  let sizey = ymax - ymin in
  map(fn [int,screenX] (int y) =>
        map (fn int (int x) =>
               let c0 = (xmin + (f32(x) * sizex) / f32(screenX),
                         ymin + (f32(y) * sizey) / f32(screenY)) in
               divergence(depth, c0)
            , iota(screenX)),
        iota(screenY))

fun [[int,screenX],screenY] main(int screenX, int screenY, int depth, (f32,f32,f32,f32) view) =
  let escapes = mandelbrot(screenX, screenY, depth, view) in
  map(fn [int,screenX] ([int] row) =>
        map(escapeToColour(depth), row),
      escapes)

-- Returns RGB (no alpha channel).
fun int escapeToColour(int depth, int divergence) =
  if depth == divergence
  then 0
  else
    let r = 3 * divergence
    let g = 5 * divergence
    let b = 7 * divergence
    in (r<<16 | g<<8 | b)

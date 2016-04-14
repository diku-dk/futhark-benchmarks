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

fun f32 dot({f32,f32} c) =
  let {r, i} = c in
  r * r + i * i

fun {f32,f32} multComplex({f32,f32} x, {f32,f32} y) =
  let {a, b} = x in
  let {c, d} = y in
  {a*c - b * d,
   a*d + b * c}

fun {f32,f32} addComplex({f32,f32} x, {f32,f32} y) =
  let {a, b} = x in
  let {c, d} = y in
  {a + c,
   b + d}

fun int divergence(int depth, {f32,f32} c0) =
  loop ({c, i} = {c0, 0}) = while i < depth && dot(c) < 4.0 do
    {addComplex(c0, multComplex(c, c)),
     i + 1} in
  i

fun [[int,height],width] mandelbrot(int width, int height, int depth, {f32,f32,f32,f32} view) =
  let {xmin, ymin, xmax, ymax} = view in
  let sizex = xmax - xmin in
  let sizey = ymax - ymin in
  map(fn [int,width] (int x) =>
        map (fn int (int y) =>
               let c0 = {xmin + (f32(x) * sizex) / f32(width),
                         ymin + (f32(y) * sizey) / f32(height)} in
               divergence(depth, c0)
            , iota(height)),
        iota(width))

fun [[[i8,3],height],width] main(int width, int height, int depth, {f32,f32,f32,f32} view) =
  let escapes = mandelbrot(width, height, depth, view) in
  map(fn [[i8],width] ([int] row) =>
        map(escapeToColour(depth), map(+1, row)),
      escapes)

-- Returns RGB (no alpha channel).
fun [i8,3] escapeToColour(int depth, int divergence) =
  if depth == divergence
  then [255i8, 0i8, 0i8]
  else
    let r = i8(3 * divergence) in
    let g = i8(5 * divergence) in
    let b = i8(7 * divergence) in
    [r, g, b]

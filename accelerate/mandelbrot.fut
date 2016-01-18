-- Port of Accelerate's Mandelbrot example.
--
-- Complicated a little bit by the fact that Futhark does not natively
-- support complex numbers.  We will represent a complex number as a
-- tuple {real,real}.

fun real dot({real,real} c) =
  let {r, i} = c in
  r * r + i * i

fun {real,real} multComplex({real,real} x, {real,real} y) =
  let {a, b} = x in
  let {c, d} = y in
  {a*c - b * d,
   a*d + b * c}

fun {real,real} addComplex({real,real} x, {real,real} y) =
  let {a, b} = x in
  let {c, d} = y in
  {a + c,
   b + d}

fun int divergence(int depth, {real,real} c0) =
  loop ({c, i} = {c0, 0}) = while i < depth && dot(c) < 4.0 do
    {addComplex(c0, multComplex(c, c)),
     i + 1} in
  i

fun [[int,screenX],screenY] mandelbrot(int screenX, int screenY, int depth, {real,real,real,real} view) =
  let {xmin, ymin, xmax, ymax} = view in
  let sizex = xmax - xmin in
  let sizey = ymax - ymin in
  map(fn [int,screenX] (int y) =>
        map (fn int (int x) =>
               let c0 = {xmin + (real(x) * sizex) / real(screenX),
                         ymin + (real(y) * sizey) / real(screenY)} in
               divergence(depth, c0)
            , iota(screenX)),
        iota(screenY))

fun [[[int,3],screenX],screenY] main(int screenX, int screenY, int depth, {real,real,real,real} view) =
  let escapes = mandelbrot(screenX, screenY, depth, view) in
  map(fn [[int,3],screenX] ([int] row) =>
        map(escapeToColour(depth), row),
      escapes)

-- Returns RGB (no alpha channel).
fun [int,3] escapeToColour(int depth, int divergence) =
  if depth == divergence
  then [0x00, 0x00, 0x00]
  else let closeness = real(divergence) / real(depth) in
       let c = trunc(255.0 * closeness) in
       [c, c, c]

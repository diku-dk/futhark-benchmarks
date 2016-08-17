default(f32)

fun f32 dot((f32,f32) c) =
  let (r, i) = c in
  r * r + i * i

fun (f32,f32) multComplex((f32,f32) x, (f32,f32) y) =
  let (a, b) = x in
  let (c, d) = y in
  (a*c - b * d,
   a*d + b * c)

fun (f32,f32) addComplex((f32,f32) x, (f32,f32) y) =
  let (a, b) = x in
  let (c, d) = y in
  (a + c,
   b + d)

fun int divergence(int limit, (f32,f32) c0) =
  loop ((c, i) = (c0, 0)) = while i < limit && dot(c) < 4.0 do
    (addComplex(c0, multComplex(c, c)),
     i + 1) in
  i

fun [width][height]int mandelbrot(int width, int height, int limit, (f32,f32,f32,f32) view) =
  let (xmin, ymin, xmax, ymax) = view in
  let sizex = xmax - xmin in
  let sizey = ymax - ymin in
  map(fn [height]int (int x) =>
        map (fn int (int y) =>
               let c0 = (xmin + (f32(x) * sizex) / f32(width),
                         ymin + (f32(y) * sizey) / f32(height)) in
               divergence(limit, c0)
            , iota(height)),
        iota(width))

fun [width][height][3]i8 main(int width, int height, int limit, f32 xmin, f32 ymin, f32 xmax, f32 ymax) =
  let escapes = mandelbrot(width, height, limit, (xmin, ymin, xmax, ymax)) in
  map(fn [height][3]i8 ([]int row) =>
        map(escapeToColour(limit), row),
      escapes)

-- Returns RGB (no alpha channel).
fun [3]i8 escapeToColour(int limit, int divergence) =
  if limit == divergence
  then [0i8, 0i8, 0i8]
  else
    let r = i8(3 * divergence) in
    let g = i8(5 * divergence) in
    let b = i8(7 * divergence) in
    [r, g, b]

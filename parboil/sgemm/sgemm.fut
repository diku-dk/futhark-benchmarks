-- The tiny dataset is something I made up - it is not from Parboil.
--
-- ==
-- input @ data/tiny.in
-- output @ data/tiny.out


fun [n][p]f32 mult([n][m]f32 xss, [m][p]f32 yss) =
  map(fn [p]f32 ([]f32 xs) =>
        map(fn f32 ([]f32 ys) =>
              reduce(+, 0f32, zipWith(*, xs, ys)),
            transpose(yss)),
      xss)

fun [n][m]f32 add([n][m]f32 xss, [n][m]f32 yss) =
  zipWith(fn [m]f32 ([]f32 xs, []f32 ys) =>
            zipWith(+, xs, ys),
          xss, yss)

fun [n][m]f32 scale([n][m]f32 xss, f32 a) =
  map(fn [m]f32 ([]f32 xs) =>
        map(*a, xs),
      xss)

fun [n][p]f32 main([n][m]f32 ass, [m][p]f32 bss, [n][p]f32 css, f32 alpha, f32 beta) =
  add(scale(css,beta), scale(mult(ass,bss), alpha))

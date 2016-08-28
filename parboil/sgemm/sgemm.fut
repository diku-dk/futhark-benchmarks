-- The tiny dataset is something I made up - it is not from Parboil.
--
-- ==
-- input @ data/tiny.in
-- output @ data/tiny.out
-- compiled input @ data/small.in
-- output @ data/small.out
-- notravis input @ data/medium.in
-- output @ data/medium.out


fun mult(xss: [n][m]f32, yss: [m][p]f32): [n][p]f32 =
  map (fn (xs: []f32): [p]f32  =>
        map (fn (ys: []f32): f32  =>
              reduce (+) 0f32 (zipWith (*) xs ys)) (
            transpose(yss))) xss

fun add(xss: [n][m]f32, yss: [n][m]f32): [n][m]f32 =
  zipWith (fn (xs: []f32, ys: []f32): [m]f32  =>
            zipWith (+) xs ys) xss yss

fun scale(xss: [n][m]f32, a: f32): [n][m]f32 =
  map (fn (xs: []f32): [m]f32  =>
        map (*a) xs) xss

fun main(ass: [n][m]f32, bss: [m][p]f32, css: [n][p]f32, alpha: f32, beta: f32): [n][p]f32 =
  add(scale(css,beta), scale(mult(ass,bss), alpha))

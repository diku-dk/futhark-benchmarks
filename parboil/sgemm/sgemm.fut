-- The tiny dataset is something I made up - it is not from Parboil.
--
-- ==
-- input @ data/tiny.in
-- output @ data/tiny.out
-- compiled input @ data/small.in
-- output @ data/small.out
-- compiled input @ data/medium.in
-- output @ data/medium.out

import "/futlib/array"

let mult [n][m][p] (xss: [n][m]f32, yss: [m][p]f32): [n][p]f32 =
  map (\(xs: []f32): [p]f32  ->
        map (\(ys: []f32): f32  ->
              reduce (+) 0f32 (map2 (*) xs ys)) (
            transpose(yss))) xss

let add [n][m] (xss: [n][m]f32, yss: [n][m]f32): [n][m]f32 =
  map2 (\(xs: []f32) (ys: []f32): [m]f32  ->
            map2 (+) xs ys) xss yss

let scale [n][m] (xss: [n][m]f32, a: f32): [n][m]f32 =
  map (\(xs: []f32): [m]f32  ->
        map (*a) xs) xss

let main [n][m][p] (ass: [n][m]f32, bss: [m][p]f32, css: [n][p]f32, alpha: f32, beta: f32): [n][p]f32 =
  add(scale(css,beta), scale(mult(ass,bss), alpha))

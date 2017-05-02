-- The tiny dataset is something I made up - it is not from Parboil.
--
-- ==
-- input @ data/tiny.in
-- output @ data/tiny.out
-- compiled input @ data/small.in
-- output @ data/small.out
-- input @ data/medium.in
-- output @ data/medium.out

import "/futlib/array"

let mult(xss: [#n][#m]f32, yss: [#m][#p]f32): [n][p]f32 =
  map (\(xs: []f32): [p]f32  ->
        map (\(ys: []f32): f32  ->
              reduce (+) 0f32 (map (*) xs ys)) (
            transpose(yss))) xss

let add(xss: [#n][#m]f32, yss: [#n][#m]f32): [n][m]f32 =
  map (\(xs: []f32) (ys: []f32): [m]f32  ->
            map (+) xs ys) xss yss

let scale(xss: [#n][#m]f32, a: f32): [n][m]f32 =
  map (\(xs: []f32): [m]f32  ->
        map (*a) xs) xss

let main(ass: [#n][#m]f32, bss: [#m][#p]f32, css: [#n][#p]f32, alpha: f32, beta: f32): [n][p]f32 =
  add(scale(css,beta), scale(mult(ass,bss), alpha))

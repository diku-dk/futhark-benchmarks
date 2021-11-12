-- The tiny dataset is something I made up - it is not from Parboil.
--
-- ==
-- input @ data/tiny.in
-- output @ data/tiny.out
-- compiled input @ data/small.in
-- output @ data/small.out
-- compiled input @ data/medium.in.gz
-- output @ data/medium.out.gz
-- compiled random input { [8192][8192]f32 [8192][8192]f32 [8192][8192]f32 0.5f32 0.1f32 }

let mult [n][m][p] (xss: [n][m]f32, yss: [m][p]f32): [n][p]f32 =
  let dotprod xs ys = f32.sum (map2 (*) xs ys)
  in map (\xs -> map (dotprod xs) (transpose yss)) xss

let add [n][m] (xss: [n][m]f32, yss: [n][m]f32): [n][m]f32 =
  map2 (map2 (+)) xss yss

let scale [n][m] (xss: [n][m]f32, a: f32): [n][m]f32 =
  map (map1 (*a)) xss

let main [n][m][p] (ass: [n][m]f32) (bss: [m][p]f32) (css: [n][p]f32)
                   (alpha: f32) (beta: f32)
                 : [n][p]f32 =
  add(scale(css,beta), scale(mult(ass,bss), alpha))

-- The tiny dataset is something I made up - it is not from Parboil.
--
-- ==
-- input @ data/tiny.in
-- output @ data/tiny.out
-- compiled input @ data/small.in
-- output @ data/small.out
-- compiled input @ data/medium.in.gz
-- output @ data/medium.out.gz

def mult [n][m][p] (xss: [n][m]f32, yss: [m][p]f32): [n][p]f32 =
  let dotprod xs ys = f32.sum (xs * ys)
  in dotprod (transpose (replicate p xss)) (replicate n (transpose yss))

def main [n][m][p] (ass: [n][m]f32) (bss: [m][p]f32) (css: [n][p]f32)
                   (alpha: f32) (beta: f32)
                 : [n][p]f32 =
  css*beta + mult(ass,bss)*alpha

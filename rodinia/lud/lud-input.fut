-- Generate well-conditioned matrix to avoid floating point errors.
--
-- Algorithm from Rodinia.

let main(n: i32): [n][n]f32 =
  let lambda = -0.001
  let f (p: i32) =
    (if p >= n
     then let i = -n + p + 1
          in 10.0 * f32.exp(lambda*r32(i))
     else let i = n - p - 1
          in 10.0 * f32.exp(lambda*r32(i)))
  in map (\i -> map (\j -> f (n-1-i+j)) (iota n))
         (iota n)

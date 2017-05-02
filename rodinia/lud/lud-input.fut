-- Generate well-conditioned matrix to avoid floating point errors.
--
-- Algorithm from Rodinia.

import "/futlib/math"

default(f32)

let main(n: i32): [n][n]f32 =
  let lambda = -0.001
  let coe = map (\j ->
                   if j >= n
                   then let i = -n + j + 1
                        in 10.0 * f32.exp(lambda*f32(i))
                   else let i = n - j - 1
                        in 10.0 * f32.exp(lambda*f32(i)))
                (iota (n*2))
  in map (\i ->
            map (\j -> coe[n-1-i+j]) (iota n))
         (iota n)

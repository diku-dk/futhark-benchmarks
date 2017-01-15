-- Generate well-conditioned matrix to avoid floating point errors.
--
-- Algorithm from Rodinia.

default(f32)

fun main(n: i32): [n][n]f32 =
  let lambda = -0.001
  let coe = map (\j ->
                   if j >= n
                   then let i = -n + j + 1
                        in 10.0 * exp32(lambda*f32(i))
                   else let i = n - j - 1
                        in 10.0 * exp32(lambda*f32(i)))
                (iota (n*2))
  in map (\i ->
            map (\j -> coe[n-1-i+j]) (iota n))
         (iota n)

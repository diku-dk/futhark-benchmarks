-- Generate well-conditioned matrix to avoid floating point errors.
--
-- Algorithm from Rodinia.

default(f32)

fun main(n: int): [n][n]f32 =
  let lambda = -0.001
  let coe = map (fn j =>
                   if j >= n
                   then let i = -n + j + 1
                        in 10.0 * exp32(lambda*f32(i))
                   else let i = n - j - 1
                        in 10.0 * exp32(lambda*f32(i)))
                (iota (n*2))
  in map (fn i =>
            map (fn j => coe[n-1-i+j]) (iota n))
         (iota n)

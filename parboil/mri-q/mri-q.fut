-- Port based on the CPU implementation in Parboil.
--
-- Anyway, performance is okayish.  On the small data set, Parboil runs in
-- 4 seconds on the CPU, and Futhark runs in 7.5 seconds.  On the GPU, on
-- the large dataset, we run in 39ms, and Parboil (CUDA) runs in something
-- like 4ms (but this does not include all memsets and allocations, whilst
-- the Futhark runtime does include some).
--
-- The biggest kernel is essentially a map-with-reduce where we just
-- sequentialise the reduction and run a map.  Parboil does essentially the
-- same thing, but they also play some tricks with constant memory.  I
-- tried replicating this in Futhark, but it did not affect performance at
-- all.  They also have an unrolled loop.  Again, I tried replicating it,
-- but it only brought us down to 32ms.
--
-- After this I got really upset and tried out Parboil's OpenCL
-- implementation, whose kernel runs in 44ms using what appears to be the
-- same algorithm.  I suspect NVIDIA/CUDA trickery!
--
-- I even tried commenting out the memory reads entirely, and it barely
-- affected the runtime.  _However_, changing the calls to sin() and cos()
-- to merely divide by 2 reduces the kernel runtime to a third.
--
-- Adding -cl-unsafe-math-optimizations to the kernel compiler command line
-- also brings down kernel runtime from 32ms to 12ms, even with the calls
-- to sin() and cos().
--
-- I am not sure what to check next.
--
-- ==
-- compiled input @ data/small.in
-- output @ data/small.out
-- notravis input @ data/large.in
-- output @ data/large.out

fun pi(): f32 = 3.1415926535897932384626433832795029f32
fun pi2(): f32 = pi() * 2.0f32

fun main(kx: [numK]f32, ky: [numK]f32, kz: [numK]f32,
                                  x: [numX]f32, y: [numX]f32, z: [numX]f32,
                                  phiR: [numK]f32, phiI: [numK]f32): ([numX]f32, [numX]f32) =
  let phiMag = zipWith(fn (r: f32, i: f32): f32  =>
                         r*r + i*i
                      , phiR, phiI) in
  let expArgs = zipWith(fn (x_e: f32, y_e: f32, z_e: f32): [numK]f32  =>
                          map(pi2()*,
                              zipWith(fn (kx_e: f32, ky_e: f32, kz_e: f32): f32  =>
                                        kx_e * x_e + ky_e * y_e + kz_e * z_e
                                     , kx, ky, kz))
                       , x, y, z) in
  let qr = map(fn (row: [numK]f32): f32  =>
                 reduce(+, 0.0f32, zipWith(*, phiMag, map(cos32, row)))
              , expArgs) in
  let qi = map(fn (row: [numK]f32): f32  =>
                 reduce(+, 0.0f32, zipWith(*, phiMag, map(sin32, row)))
              , expArgs) in
  (qr, qi)

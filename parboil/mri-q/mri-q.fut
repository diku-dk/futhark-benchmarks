-- Port based on the CPU implementation in Parboil.
--
-- Note that testing fails due to floating-point imprecision.  In the
-- Parboil test suite, the tolerance is at least 0.2% (and scales up
-- with the data set).  There is presently no way to express this in
-- the Futhark test runner.
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
-- compiled input @ data/large.in
-- output @ data/large.out

fun f32 pi() = 3.1415926535897932384626433832795029f
fun f32 pi2() = pi() * 2.0f

fun {[f32,numX], [f32,numX]} main([f32,numK] kx, [f32,numK] ky, [f32,numK] kz,
                                  [f32,numX] x, [f32,numX] y, [f32,numX] z,
                                  [f32,numK] phiR, [f32,numK] phiI) =
  let phiMag = zipWith(fn f32 (f32 r, f32 i) =>
                         r*r + i*i
                      , phiR, phiI) in
  let {cosArgs, sinArgs} =
    unzip(
      zipWith(fn {[f32,numK], [f32,numK]} (f32 x_e, f32 y_e, f32 z_e) =>
                let expArg =
                  map(pi2()*,
                      zipWith(fn f32 (f32 kx_e, f32 ky_e, f32 kz_e) =>
                                kx_e * x_e + ky_e * y_e + kz_e * z_e
                             , kx, ky, kz)) in
                {map(cos32, expArg), map(sin32, expArg)}
             , x, y, z)) in
  let Qr = map(fn f32 ([f32,numK] cosRow) =>
                 reduce(+, 0.0f, zipWith(*, phiMag, cosRow))
              , cosArgs) in
  let Qi = map(fn f32 ([f32,numK] sinRow) =>
                 reduce(+, 0.0f, zipWith(*, phiMag, sinRow))
              , sinArgs) in
  {Qr, Qi}

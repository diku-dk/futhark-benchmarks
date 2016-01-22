-- Port based on the CPU implementation in Parboil.
--
-- Note that testing fails due to floating-point imprecision.  In the
-- Parboil test suite, the tolerance is at least 0.2% (and scales up
-- with the data set).  There is presently no way to express this in
-- the Futhark test runner.
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

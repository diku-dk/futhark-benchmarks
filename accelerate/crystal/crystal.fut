-- The first data set is equal to the Accelerate default.
--
-- ==
-- tags { futhark-c futhark-opencl }
-- notravis input {
--   200
--   30.0f32
--   5
--   1
--   1.0f32
-- }
--
-- compiled input {
--   20
--   30.0f32
--   5
--   50
--   0.5f32
-- }
--
-- compiled input {
--   40
--   30.0f32
--   5
--   50
--   0.5f32
-- }
--
-- compiled input {
--   40
--   30.0f32
--   50
--   50
--   0.5f32
-- }
--
-- notravis input {
--   2000
--   30.0f32
--   50
--   1
--   1.0f32
-- }
--
-- notravis input {
--   4000
--   30.0f32
--   50
--   1
--   1.0f32
-- }

default (f32)

fun f32 pi() = 3.14159265358979323846264338327950288419716939937510

fun bool odd(i32 n) = (n & 1) == 1

fun u32 quasicrystal(f32 scale, i32 degree, f32 time, f32 x, f32 y) =
  let phi = 1.0 + (time ** 1.5) * 0.005 in
  let (x', y') = point(scale, x, y) in
  intColour(rampColour(waves(degree, phi, x', y')))

fun f32 waves(i32 degree, f32 phi, f32 x, f32 y) =
  let th = pi() / phi in
  wrap(waver(th, x, y, degree, 0.0))

fun f32 waver(f32 th, f32 x, f32 y, i32 n, f32 acc) =
  reduce(+, 0.0, (map(fn f32 (i32 i) => wave(f32(i) * th, x, y), iota(n))))

fun f32 wrap(f32 n) =
  let n_ = i32(n) in
  let n' = n - f32(n_) in
  -- if odd(n_) then 1.0 - n' else n'
  let odd_in_int = n_ & 1 in
  let even_in_int = 1 - odd_in_int in
  f32(odd_in_int) * (1.0 - n') + f32(even_in_int) * n'

fun f32 wave(f32 th, f32 x, f32 y) =
  let cth = cos32(th) in
  let sth = sin32(th) in
  (cos32(cth * x + sth * y) + 1.0) / 2.0

fun (f32, f32) point(f32 scale, f32 x, f32 y) =
  (x * scale, y * scale)

fun (f32, f32, f32) rampColour(f32 v) =
  (1.0, 0.4 + (v * 0.6), v) -- rgb

fun u32 intColour((f32, f32, f32) rgb) =
  let (r, g, b) = rgb in
  u32(intPixel(r)) << 16u32 | u32(intPixel(g)) << 8u32 | u32(intPixel(b))

fun u8 intPixel(f32 t) =
  u8(255.0 * t)

fun f32 normalize_index(i32 i, i32 field_size) =
  f32(i) / f32(field_size)

fun [n_steps][field_size][field_size]u32
  main(i32 field_size, f32 scale, i32 degree,
       i32 n_steps, f32 time_delta) =
  let ks = iota(field_size) in
  map(fn [field_size][field_size]u32 (i32 step_i) =>
        let time = f32(step_i) * time_delta in
        map(fn [field_size]u32 (i32 y) =>
              map(fn u32 (i32 x) =>
                    quasicrystal(scale, degree, time,
                                 normalize_index(x, field_size),
                                 normalize_index(y, field_size)),
                  ks),
            ks),
      iota(n_steps))

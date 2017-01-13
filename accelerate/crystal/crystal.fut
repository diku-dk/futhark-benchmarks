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

fun pi(): f32 = 3.14159265358979323846264338327950288419716939937510

fun odd(n: i32): bool = (n & 1) == 1

fun quasicrystal(scale: f32, degree: i32, time: f32, x: f32, y: f32): u32 =
  let phi = 1.0 + (time ** 1.5) * 0.005
  let (x', y') = point(scale, x, y)
  in intColour(rampColour(waves(degree, phi, x', y')))

fun waves(degree: i32, phi: f32, x: f32, y: f32): f32 =
  let th = pi() / phi
  in wrap(waver(th, x, y, degree))

fun waver(th: f32, x: f32, y: f32, n: i32): f32 =
  reduce (+) (0.0) (map (\i  -> wave(f32(i) * th, x, y)) (iota n))

fun wrap(n: f32): f32 =
  let n' = n - f32(i32(n))
  let odd_in_int = i32(n) & 1
  let even_in_int = 1 - odd_in_int
  in f32(odd_in_int) * (1.0 - n') + f32(even_in_int) * n'

fun wave(th: f32, x: f32, y: f32): f32 =
  let cth = cos32(th)
  let sth = sin32(th)
  in (cos32(cth * x + sth * y) + 1.0) / 2.0

fun point(scale: f32, x: f32, y: f32): (f32, f32) =
  (x * scale, y * scale)

fun rampColour(v: f32): (f32, f32, f32) =
  (1.0, 0.4 + (v * 0.6), v) -- rgb

fun intColour((r,g,b): (f32, f32, f32)): u32 =
  u32(intPixel(r)) << 16u32 | u32(intPixel(g)) << 8u32 | u32(intPixel(b))

fun intPixel(t: f32): u8 =
  u8(255.0 * t)

fun normalize_index(i: i32, field_size: i32): f32 =
  f32(i) / f32(field_size)

fun main(field_size: i32, scale: f32, degree: i32,
         n_steps: i32, time_delta: f32): [n_steps][field_size][field_size]u32 =
  map (\step_i: [field_size][field_size]u32  ->
         let time = f32(step_i) * time_delta
         in render_frame(field_size, scale, degree, time))
  (iota(n_steps))

entry render_frame(field_size: i32, scale: f32, degree: i32, time: f32)
                  : [field_size][field_size]u32 =
  let ks = iota(field_size)
  in map (\(y: i32): [field_size]u32  ->
            map (\(x: i32): u32  ->
                   quasicrystal(scale, degree, time,
                                normalize_index(x, field_size),
                                normalize_index(y, field_size)))
                ks)
         ks

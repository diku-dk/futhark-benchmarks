fun f64 pi() = 3.14159265358979323846264338327950288419716939937510

fun bool odd(i32 n) = (n & 1) == 1

fun [i32, 3] quasicrystal(f64 scale, i32 degree, f64 time, f64 x, f64 y) =
  let phi = 1.0 + (time ** 1.5) * 0.005 in
  let {x', y'} = point(scale, x, y) in
  intColour(rampColour(waves(degree, phi, x', y')))

fun f64 waves(i32 degree, f64 phi, f64 x, f64 y) =
  let th = pi() / phi in
  wrap(waver(th, x, y, degree, 0.0))

fun f64 waver(f64 th, f64 x, f64 y, i32 n, f64 acc) =
  reduce(+, 0.0, (map(fn f64 (i32 i) => wave(f64(i) * th, x, y), iota(n))))

fun f64 wrap(f64 n) =
  let n_ = i32(n) in
  let n' = n - f64(n_) in
  -- if odd(n_) then 1.0 - n' else n'
  let odd_in_int = n_ & 1 in
  let even_in_int = 1 - odd_in_int in
  f64(odd_in_int) * (1.0 - n') + f64(even_in_int) * n'

fun f64 wave(f64 th, f64 x, f64 y) =
  let cth = cos(th) in
  let sth = sin(th) in
  (cos (cth * x + sth * y) + 1.0) / 2.0

fun {f64, f64} point(f64 scale, f64 x, f64 y) =
  {x * scale, y * scale}

fun {f64, f64, f64} rampColour(f64 v) =
  {1.0, 0.4 + (v * 0.6), v} -- rgb

fun [i32, 3] intColour({f64, f64, f64} rgb) =
  let {r, g, b} = rgb in
  [intPixel(r), intPixel(g), intPixel(b)]

fun i32 intPixel(f64 t) =
  i32(255.0 * t)

fun f64 normalize_index(i32 i, i32 field_size) =
  f64(i) / f64(field_size)
  
fun [[[[i32, 3], field_size], field_size], n_steps]
  main(i32 field_size, f64 scale, i32 degree,
       i32 n_steps, f64 time_delta) =
  let ks = iota(field_size) in
  map(fn [[[i32, 3], field_size], field_size] (i32 step_i) =>
        let time = f64(step_i) * time_delta in
        map(fn [[i32, 3], field_size] (i32 y) =>
              map(fn [i32, 3] (i32 x) =>
                    quasicrystal(scale, degree, time,
                                 normalize_index(x, field_size),
                                 normalize_index(y, field_size)),
                  ks),
            ks),
      iota(n_steps))

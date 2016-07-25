--
-- ==
-- tags { disable }

include fluid

fun i8 clamp(f32 x) =
  if x < 0.0f32
  then 0i8
  else if x > 255.0f32
  then 255i8
  else i8(x)

fun [g_minus_two][g_minus_two][3]i8
  draw_densities([g][g]f32 ds,
                 i32 g_minus_two) =
  let ks = map(fn i32 (i32 k) => k + 1, iota(g_minus_two)) in
  map(fn [g_minus_two][3]i8 (i32 i) =>
        map(fn [3]i8 (i32 j) =>
              let value = clamp(255.0f32 * unsafe ds[i, j])
              in [value, value, value],
            ks),
        ks)

fun ([g_minus_two][g_minus_two][3]i8,
     [g][g]f32,
     [g][g]f32,
     [g][g]f32)
  draw_one_frame([g][g]f32 u0,
                 [g][g]f32 v0,
                 [g][g]f32 d0,
                 i32 n_solver_steps,
                 f32 time_step,
                 f32 diffusion_rate,
                 f32 viscosity,
                 i32 g_minus_two) =
  let (u1, v1, d1) = step(u0, v0, d0, n_solver_steps,
                       time_step, diffusion_rate, viscosity) in
  (draw_densities(d1, g_minus_two), u1, v1, d1)

fun ([][][3]i8,
     [g][g]f32,
     [g][g]f32,
     [g][g]f32)
  main([g][g]f32 u0,
       [g][g]f32 v0,
       [g][g]f32 d0,
       i32 n_solver_steps,
       f32 time_step,
       f32 diffusion_rate,
       f32 viscosity) =
  draw_one_frame(u0, v0, d0, n_solver_steps,
                 time_step, diffusion_rate, viscosity, g - 2)

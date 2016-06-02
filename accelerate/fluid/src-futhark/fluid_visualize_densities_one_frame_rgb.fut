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

fun [[[i8, 3], g_minus_two], g_minus_two]
  draw_densities([[f32, g], g] D,
                 i32 g_minus_two) =
  let ks = map(fn i32 (i32 k) => k + 1, iota(g_minus_two)) in
  map(fn [[i8, 3], g_minus_two] (i32 i) =>
        map(fn [i8, 3] (i32 j) =>
              let value = clamp(255.0f32 * unsafe D[i, j])
              in [value, value, value],
            ks),
        ks)

fun ([[[i8, 3], g_minus_two], g_minus_two],
     [[f32, g], g],
     [[f32, g], g],
     [[f32, g], g])
  draw_one_frame([[f32, g], g] U0,
                 [[f32, g], g] V0,
                 [[f32, g], g] D0,
                 i32 n_solver_steps,
                 f32 time_step,
                 f32 diffusion_rate,
                 f32 viscosity,
                 i32 g_minus_two) =
  let (U1, V1, D1) = step(U0, V0, D0, n_solver_steps,
                       time_step, diffusion_rate, viscosity) in
  (draw_densities(D1, g_minus_two), U1, V1, D1)

fun ([[[i8, 3]]],
     [[f32, g], g],
     [[f32, g], g],
     [[f32, g], g])
  main([[f32, g], g] U0,
       [[f32, g], g] V0,
       [[f32, g], g] D0,
       i32 n_solver_steps,
       f32 time_step,
       f32 diffusion_rate,
       f32 viscosity) =
  draw_one_frame(U0, V0, D0, n_solver_steps,
                 time_step, diffusion_rate, viscosity, g - 2)

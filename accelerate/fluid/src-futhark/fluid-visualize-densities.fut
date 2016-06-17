--
-- ==
-- tags { notravis }

include fluid

fun i32 clamp(f32 x) =
  if x < 0.0f32
  then 0i32
  else if x > 255.0f32
  then 255i32
  else i32(x)

fun [g_minus_two][g_minus_two]i32
  draw_densities([g][g]f32 d,
                 i32 g_minus_two) =
  let ks = map(fn i32 (i32 k) => k + 1, iota(g - 2)) in
  map(fn [g_minus_two]i32 (i32 i) =>
        map(fn i32 (i32 j) =>
              clamp(255.0f32 * unsafe d[i, j]),
            ks),
        ks)

fun [n_steps][g_minus_two][g_minus_two]i32
  draw_all_frames([g][g]f32 u0,
                  [g][g]f32 v0,
                  [g][g]f32 d0,
                  i32 n_steps,
                  i32 n_solver_steps,
                  f32 time_step,
                  f32 diffusion_rate,
                  f32 viscosity,
                  i32 g_minus_two) =
  let (us, vs, ds) = get_all_frames(u0, v0, d0, n_steps, n_solver_steps,
                                    time_step, diffusion_rate, viscosity) in
  map(fn [g_minus_two][g_minus_two]i32 ([g][g]f32 d) =>
        draw_densities(d, g - 2), ds)

-- fun [n_steps][g_minus_two][g_minus_two]i32
fun [n_steps][][]i32
  main([g][g]f32 u0,
       [g][g]f32 v0,
       [g][g]f32 d0,
       i32 n_steps,
       i32 n_solver_steps,
       f32 time_step,
       f32 diffusion_rate,
       f32 viscosity) =
  draw_all_frames(u0, v0, d0, n_steps, n_solver_steps,
                  time_step, diffusion_rate, viscosity, g - 2)

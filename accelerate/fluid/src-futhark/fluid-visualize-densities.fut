include fluid

fun i32 clamp(f32 x) =
  if x < 0.0f32
  then 0i32
  else if x > 255.0f32
  then 255i32
  else i32(x)

fun [[i32, g_minus_two], g_minus_two]
  draw_densities([[f32, g], g] D,
                 i32 g_minus_two) =
  let ks = map(fn i32 (i32 k) => k + 1, iota(g - 2)) in
  map(fn [i32, g_minus_two] (i32 i) =>
        map(fn i32 (i32 j) =>
              clamp(255.0f32 * unsafe D[i, j]),
            ks),
        ks)

fun [[[i32, g_minus_two], g_minus_two], n_steps]
  draw_all_frames([[f32, g], g] U0,
                  [[f32, g], g] V0,
                  [[f32, g], g] D0,
                  i32 n_steps,
                  f32 time_step,
                  f32 diffusion_rate,
                  f32 viscosity,
                  i32 g_minus_two) =
  let {Us, Vs, Ds} = get_all_frames(U0, V0, D0, n_steps,
                                    time_step, diffusion_rate, viscosity) in
  map(fn [[i32, g_minus_two], g_minus_two] ([[f32, g], g] D) =>
        draw_densities(D, g - 2), Ds)

-- fun [[[i32, g_minus_two], g_minus_two], n_steps]
fun [[[i32]], n_steps]
  main([[f32, g], g] U0,
       [[f32, g], g] V0,
       [[f32, g], g] D0,
       i32 n_steps,
       f32 time_step,
       f32 diffusion_rate,
       f32 viscosity) =
  draw_all_frames(U0, V0, D0, n_steps, time_step, diffusion_rate, viscosity,
                  g - 2)

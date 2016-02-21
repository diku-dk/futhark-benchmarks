--!import fluid-lib

fun [[int, g_minus_two], g_minus_two]
  draw_densities([[real, g], g] D,
                 int g_minus_two) =
  let ks = map(fn int (int k) => k + 1, iota(g - 2)) in
  map(fn [int, g_minus_two] (int i) =>
        map(fn int (int j) =>
              int(255.0 * D[i, j]),
            ks),
        ks)

fun [[[int, g_minus_two], g_minus_two], n_steps]
  draw_all_frames([[real, g], g] U0,
                  [[real, g], g] V0,
                  [[real, g], g] D0,
                  int n_steps,
                  real time_step,
                  real diffusion_rate,
                  real viscosity,
                  int g_minus_two) =
  let {Us, Vs, Ds} = get_all_frames(U0, V0, D0, n_steps,
                                    time_step, diffusion_rate, viscosity) in
  map(fn [[int, g_minus_two], g_minus_two] ([[real, g], g] D) =>
        draw_densities(D, g - 2), Ds)

-- fun [[[int, g_minus_two], g_minus_two], n_steps]
fun [[[int]], n_steps]
  main([[real, g], g] U0,
       [[real, g], g] V0,
       [[real, g], g] D0,
       int n_steps,
       real time_step,
       real diffusion_rate,
       real viscosity) =
  draw_all_frames(U0, V0, D0, n_steps, time_step, diffusion_rate, viscosity,
                  g - 2)

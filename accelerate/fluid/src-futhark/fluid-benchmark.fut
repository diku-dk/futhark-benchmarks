--
-- ==
-- tags { notravis }
-- compiled input @ fluid-n_steps40-grid_res100.input

include fluid

fun {[[f32, g], g],
     [[f32, g], g],
     [[f32, g], g]}
  main([[f32, g], g] U0,
       [[f32, g], g] V0,
       [[f32, g], g] D0,
       i32 n_steps,
       f32 time_step,
       f32 diffusion_rate,
       f32 viscosity) =
  get_end_frame(U0, V0, D0, n_steps, time_step, diffusion_rate, viscosity)

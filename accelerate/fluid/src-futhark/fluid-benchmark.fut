--
-- ==
-- tags { futhark-opencl futhark-c }
-- compiled input @ fluid-n_steps=1-n_solver_steps=40-grid_res=100.input

include fluid

fun ([[f32, g], g],
     [[f32, g], g],
     [[f32, g], g])
  main([[f32, g], g] u0,
       [[f32, g], g] v0,
       [[f32, g], g] d0,
       i32 n_steps,
       i32 n_solver_steps,
       f32 time_step,
       f32 diffusion_rate,
       f32 viscosity) =
  get_end_frame(u0, v0, d0, n_steps, n_solver_steps,
                time_step, diffusion_rate, viscosity)

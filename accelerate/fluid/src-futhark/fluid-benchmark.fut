-- Benchmark the rendering code without drawing it to the screen.
-- ==
-- tags { futhark-opencl futhark-c }
-- compiled input @ medium.in

-- medium.in attributes: n_steps=1, n_solver_steps=40, grid_res=100

import "fluid"

let main [g] (u0: [g][g]f32,
              v0: [g][g]f32,
              d0: [g][g]f32,
              n_steps: i32,
              n_solver_steps: i32,
              time_step: f32,
              diffusion_rate: f32,
              viscosity: f32): ([g][g]f32,
                                [g][g]f32,
                                [g][g]f32) =
  get_end_frame(u0, v0, d0, n_steps, n_solver_steps,
                time_step, diffusion_rate, viscosity)

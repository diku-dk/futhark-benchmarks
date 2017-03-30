--
-- ==
-- tags { }

import "fluid"

let clamp(x: f32): i32 =
  if x < 0.0f32
  then 0i32
  else if x > 255.0f32
  then 255i32
  else i32(x)

let draw_densities(d: [g][g]f32,
                 g_minus_two: i32): [g_minus_two][g_minus_two]i32 =
  let ks = map (\(k: i32): i32  -> k + 1) (iota(g - 2))
  in map (\(i: i32): [g_minus_two]i32  ->
            map (\(j: i32): i32  ->
                   clamp(255.0f32 * unsafe d[i, j])) ks) ks

let draw_all_frames(u0: [g][g]f32,
                  v0: [g][g]f32,
                  d0: [g][g]f32,
                  n_steps: i32,
                  n_solver_steps: i32,
                  time_step: f32,
                  diffusion_rate: f32,
                  viscosity: f32,
                  g_minus_two: i32): [n_steps][g_minus_two][g_minus_two]i32 =
  let (us, vs, ds) = get_all_frames(u0, v0, d0, n_steps, n_solver_steps,
                                    time_step, diffusion_rate, viscosity)
  in map (\(d: [g][g]f32): [g_minus_two][g_minus_two]i32  ->
            draw_densities(d, g - 2)) ds

-- let [n_steps][g_minus_two][g_minus_two]i32
let main(u0: [g][g]f32,
       v0: [g][g]f32,
       d0: [g][g]f32,
       n_steps: i32,
       n_solver_steps: i32,
       time_step: f32,
       diffusion_rate: f32,
       viscosity: f32): [n_steps][][]i32 =
  draw_all_frames(u0, v0, d0, n_steps, n_solver_steps,
                  time_step, diffusion_rate, viscosity, g - 2)

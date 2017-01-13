--
-- ==
-- tags { disable }

include fluid

fun clamp(x: f32): i8 =
  if x < 0.0f32
  then 0i8
  else if x > 255.0f32
  then 255i8
  else i8(x)

fun draw_densities(ds: [g][g]f32,
                 g_minus_two: i32): [g_minus_two][g_minus_two][3]i8 =
  let ks = map (\(k: i32): i32  -> k + 1) (iota(g_minus_two))
  in map (\(i: i32): [g_minus_two][3]i8  ->
            map (\(j: i32): [3]i8  ->
                   let value = clamp(255.0f32 * unsafe ds[i, j])
                   in [value, value, value]) ks) ks

fun draw_one_frame(u0: [g][g]f32,
                 v0: [g][g]f32,
                 d0: [g][g]f32,
                 n_solver_steps: i32,
                 time_step: f32,
                 diffusion_rate: f32,
                 viscosity: f32,
                 g_minus_two: i32): ([g_minus_two][g_minus_two][3]i8,
     [g][g]f32,
     [g][g]f32,
     [g][g]f32) =
  let (u1, v1, d1) = step(u0, v0, d0, n_solver_steps,
                       time_step, diffusion_rate, viscosity)
  in (draw_densities(d1, g_minus_two), u1, v1, d1)

fun main(u0: [g][g]f32,
       v0: [g][g]f32,
       d0: [g][g]f32,
       n_solver_steps: i32,
       time_step: f32,
       diffusion_rate: f32,
       viscosity: f32): ([][][3]i8,
     [g][g]f32,
     [g][g]f32,
     [g][g]f32) =
  draw_one_frame(u0, v0, d0, n_solver_steps,
                 time_step, diffusion_rate, viscosity, g - 2)

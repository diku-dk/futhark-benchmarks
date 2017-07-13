-- Fluid simulation library.
--
-- A port of Accelerate's version:
-- https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/fluid
-- (mostly based on the C version, since it was simpler).
--
-- Variable naming conventions:
--
--   + `g`: The grid resolution.  The fluid simulation works on a square grid of
--          size `g * g`.
--   + `i`: the horizontal index.
--   + `j`: the vertical index.
--   + `S*`: Some array, purpose unknown.
--   + `U*`: Array of horizontal forces.
--   + `V*`: Array of vertical forces.
--   + `D*`: Array of densities.
--
-- The simulation handles the 1-size border around the `(g - 2) * (g - 2)` grid
-- differently from the inner content.  The border depends on the outermost
-- inner values.  The original C version writes this border after writing the
-- inner values, by reading from the array.  We would like to handle this
-- pattern with a single map, so the Futhark port instead *calculates* the
-- outermost inner values when it needs them for the outer bound values, which
-- means that a few calculations are done twice.  The alternative would be to
-- first calculate all the inner values, and then write the outer values
-- afterwards.
--
-- This file has no main function, only fluid simulation library functions.  Two
-- Futhark programs use this library:
--
--   + `fluid-visualize-densities.fut`: Generate `n_steps` frames with the
--     evolving densities.
--   + `fluid-measure.fut`: Calculate the resulting densities and forces after
--     `n_steps`, not spending memory to store the intermediate frames.
--
-- The different `main` functions take the same arguments.

------------------------------------------------------------
-- General helper functions.
------------------------------------------------------------

import "/futlib/array"

let inside(i: i32, j: i32, g: i32): bool =
  i >= 1 && i <= g - 2
  && j >= 1 && j <= g - 2

let in_outside_corner(i: i32, j: i32, g: i32): bool =
  (i == 0 || i == g - 1) && (j == 0 || j == g - 1)

let corner_index_neighbors(i: i32, j: i32, g: i32): (i32, i32, i32, i32) =
  if i == 0 && j == 0
  then (1, 0, 0, 1)
  else if i == 0 && j == g - 1
  then (1, g - 1, 0, g - 2)
  else if i == g - 1 && j == 0
  then (g - 2, 0, g - 1, 1)
  else (g - 2, g - 1, g - 1, g - 2) -- if i == g - 1 && j == g - 1

let outermost_inner_index(i: i32, j: i32, g: i32, b: i32): (i32, i32, f32) =
  if i == 0
  then (1, j, if b == 1 then -1.0f32 else 1.0f32)
  else if i == g - 1
  then (g - 2, j, if b == 1 then -1.0f32 else 1.0f32)
  else if j == 0
  then (i, 1, if b == 2 then -1.0f32 else 1.0f32)
  else if j == g - 1
  then (i, g - 2, if b == 2 then -1.0f32 else 1.0f32)
  else (0, 0, 0.0f32) -- This is not supposed to happen.


------------------------------------------------------------
-- lin_solve.
------------------------------------------------------------

let lin_solve_inner(i: i32,
                    j: i32,
                    s0: [#g][#g]f32,
                    s1: [#g][#g]f32,
                    a: f32,
                    c: f32): f32 =
  -- A stencil.
  unsafe ((s0[i, j] + a *
           (s1[(i - 1), j]
            + s1[(i + 1), j]
            + s1[i, (j - 1)]
            + s1[i, (j + 1)])) / c)

let lin_solve_outer_base(i: i32,
                         j: i32,
                         s0: [#g][#g]f32,
                         s1: [#g][#g]f32,
                         a: f32,
                         c: f32,
                         b: i32): f32 =
  let (i1, j1, f) = outermost_inner_index(i, j, g, b)
  in f * lin_solve_inner(i1, j1, s0, s1, a, c)

let lin_solve_outer(i: i32,
                    j: i32,
                    s0: [#g][#g]f32,
                    s1: [#g][#g]f32,
                    a: f32,
                    c: f32,
                    b: i32): f32 =
  if in_outside_corner(i, j, g)
  then let (i1, j1, i2, j2) = corner_index_neighbors(i, j, g)
       in 0.5f32 * (lin_solve_outer_base(i1, j1, s0, s1, a, c, b)
                    + lin_solve_outer_base(i2, j2, s0, s1, a, c, b))
  else lin_solve_outer_base(i, j, s0, s1, a, c, b)

let lin_solve(n_solver_steps: i32,
              s0: [#g][#g]f32,
              b: i32,
              a: f32,
              c: f32): [g][g]f32 =
  let one = (g*g+2*g+1)/(g+1) - g
  in loop s1 = replicate g (replicate g 0.0f32) for _k < n_solver_steps do
    reshape (g, g)
    (map (\(ij: i32): f32  ->
            let i = ij / g
            let j = ij % g
            in if inside(i, j, g)
               then lin_solve_inner(i, j, s0, s1, a, c)
               else lin_solve_outer(i/one, j/one, s0, s1, a, c, b/one)
         ) (iota(g * g)))

------------------------------------------------------------
-- diffuse.
------------------------------------------------------------

let diffuse(s: [#g][#g]f32,
            b: i32,
            n_solver_steps: i32,
            diffusion_rate_or_viscosity: f32,
            time_step: f32): [g][g]f32 =
  let a = (time_step * diffusion_rate_or_viscosity
           * f32(g - 2) * f32(g - 2))
  in lin_solve(n_solver_steps, s, b, a, 1.0f32 + 4.0f32 * a)


------------------------------------------------------------
-- advect.
------------------------------------------------------------

let advect_inner(i: i32,
                 j: i32,
                 s: [#g][#g]f32,
                 u: [#g][#g]f32,
                 v: [#g][#g]f32,
                 time_step0: f32): f32 =
  let x = f32(i) - time_step0 * unsafe u[i, j]
  let y = f32(j) - time_step0 * unsafe v[i, j]

  let x = if x < 0.5f32 then 0.5f32 else x
  let x = if x > f32(g - 2) + 0.5f32 then f32(g - 2) + 0.5f32 else x
  let i0 = i32(x)
  let i1 = i0 + 1

  let y = if y < 0.5f32 then 0.5f32 else y
  let y = if y > f32(g - 2) + 0.5f32 then f32(g - 2) + 0.5f32 else y
  let j0 = i32(y)
  let j1 = j0 + 1

  let s1 = x - f32(i0)
  let s0 = 1.0f32 - s1
  let t1 = y - f32(j0)
  let t0 = 1.0f32 - t1

  in unsafe (s0 * (t0 * s[i0, j0] + t1 * s[i0, j1])
             + s1 * (t0 * s[i1, j0] + t1 * s[i1, j1]))

let advect_outer_base(i: i32,
                      j: i32,
                      s: [#g][#g]f32,
                      u: [#g][#g]f32,
                      v: [#g][#g]f32,
                      time_step0: f32,
                      b: i32): f32 =
  let (i1, j1, f) = outermost_inner_index(i, j, g, b)
  in f * advect_inner(i1, j1, s, u, v, time_step0)

let advect_outer(i: i32,
                 j: i32,
                 s: [#g][#g]f32,
                 u: [#g][#g]f32,
                 v: [#g][#g]f32,
                 time_step0: f32,
                 b: i32): f32 =
  if in_outside_corner(i, j, g)
  then let (i1, j1, i2, j2) = corner_index_neighbors(i, j, g)
       in 0.5f32 * (advect_outer_base(i1, j1, s, u, v, time_step0, b)
                    + advect_outer_base(i2, j2, s, u, v, time_step0, b))
  else advect_outer_base(i, j, s, u, v, time_step0, b)

let advect(s0: [#g][#g]f32,
           u: [#g][#g]f32,
           v: [#g][#g]f32,
           b: i32,
           time_step: f32): *[g][g]f32 =
  let one = (g*g+2*g+1)/(g+1) - g
  let time_step0 = time_step * f32(g - 2)
  in reshape(g, g)
  (map (\(ij: i32): f32  ->
          let i = ij / g
          let j = ij % g
          in if inside(i, j, g)
             then advect_inner(i, j, s0, u, v, time_step0)
             else advect_outer(i/one, j/one, s0, u, v, time_step0, b/one))
   (iota(g * g)))

------------------------------------------------------------
-- project.
------------------------------------------------------------


let project_top_inner(i: i32,
                      j: i32,
                      u0: [#g][#g]f32,
                      v0: [#g][#g]f32): f32 =
  unsafe (-0.5f32 * (u0[i + 1, j]
                     - u0[i - 1, j]
                     + v0[i, j + 1]
                     - v0[i, j - 1]) / f32(g))

let project_top_outer_base(i: i32,
                           j: i32,
                           u0: [#g][#g]f32,
                           v0: [#g][#g]f32): f32 =
  let (i1, j1, _f) = outermost_inner_index(i, j, g, 0)
  in project_top_inner(i1, j1, u0, v0)

let project_top_outer(i: i32,
                      j: i32,
                      u0: [#g][#g]f32,
                      v0: [#g][#g]f32): f32 =
  if in_outside_corner(i, j, g)
  then let (i1, j1, i2, j2) = corner_index_neighbors(i, j, g)
       in 0.5f32 * (project_top_outer_base(i1, j1, u0, v0)
                    + project_top_outer_base(i2, j2, u0, v0))
  else project_top_outer_base(i, j, u0, v0)

let project_top(u0: [#g][#g]f32,
                v0: [#g][#g]f32): [g][g]f32 =
  let one = (g*g+2*g+1)/(g+1) - g in
  reshape (g, g)
  (map (\(ij: i32): f32  ->
          let i = ij / g
          let j = ij % g
          in if inside(i, j, g)
             then project_top_inner(i, j, u0, v0)
             else project_top_outer(i/one, j/one, u0, v0)
       ) (iota(g * g)))


let project_bottom_inner(i: i32,
                         j: i32,
                         p0: [#g][#g]f32,
                         s0: [#g][#g]f32,
                         i0d: i32,
                         j0d: i32,
                         i1d: i32,
                         j1d: i32): f32 =
  unsafe (s0[i, j] - 0.5f32 * f32(g - 2)
          * (p0[i + i0d, j + j0d] - p0[i + i1d, j + j1d]))

let project_bottom_outer_base(i: i32,
                              j: i32,
                              p0: [#g][#g]f32,
                              s0: [#g][#g]f32,
                              i0d: i32,
                              j0d: i32,
                              i1d: i32,
                              j1d: i32,
                              b: i32): f32 =
  let (i1, j1, f) = outermost_inner_index(i, j, g, b)
  in f * project_bottom_inner(i1, j1, p0, s0, i0d, j0d, i1d, j1d)

let project_bottom_outer(i: i32,
                         j: i32,
                         p0: [#g][#g]f32,
                         s0: [#g][#g]f32,
                         i0d: i32,
                         j0d: i32,
                         i1d: i32,
                         j1d: i32,
                         b: i32): f32 =
  if in_outside_corner(i, j, g)
  then let (i1, j1, i2, j2) = corner_index_neighbors(i, j, g)
       in 0.5f32 * (project_bottom_outer_base(i1, j1, p0, s0, i0d, j0d, i1d, j1d, b)
                    + project_bottom_outer_base(i2, j2, p0, s0, i0d, j0d, i1d, j1d, b))
  else project_bottom_outer_base(i, j, p0, s0, i0d, j0d, i1d, j1d, b)

let project_bottom(p0: [#g][#g]f32,
                   s0: [#g][#g]f32,
                   b: i32,
                   i0d: i32,
                   j0d: i32,
                   i1d: i32,
                   j1d: i32): *[g][g]f32 =
  let one = (g*g+2*g+1)/(g+1) - g in
  reshape (g, g)
  (map (\(ij: i32): f32  ->
          let i = ij / g
          let j = ij % g
          in if inside(i, j, g)
             then project_bottom_inner(i, j, p0, s0, i0d, j0d, i1d, j1d)
             else project_bottom_outer(i/one, j/one, p0, s0,
                                       i0d/one, j0d/one, i1d/one, j1d/one, b/one)
       ) (iota(g * g)))

let project(n_solver_steps: i32,
            u0: [#g][#g]f32,
            v0: [#g][#g]f32): (*[g][g]f32, *[g][g]f32) =
  let div0 = project_top(u0, v0)
  let p0 = lin_solve(n_solver_steps, div0, 0, 1.0f32, 4.0f32)
  let u1 = project_bottom(p0, u0, 1, 1, 0, -1, 0)
  let v1 = project_bottom(p0, v0, 2, 0, 1, 0, -1)
  in (u1, v1)

------------------------------------------------------------
-- Step functions.
------------------------------------------------------------

let dens_step(d0: [#g][#g]f32,
              u0: [#g][#g]f32,
              v0: [#g][#g]f32,
              n_solver_steps: i32,
              diffusion_rate: f32,
              time_step: f32): *[g][g]f32 =
  let d1 = diffuse(d0, 0, n_solver_steps, diffusion_rate, time_step)
  let d2 = advect(d1, u0, v0, 0, time_step)
  in d2

let vel_step(u0: [#g][#g]f32,
             v0: [#g][#g]f32,
             n_solver_steps: i32,
             viscosity: f32,
             time_step: f32): (*[g][g]f32,
                               *[g][g]f32) =
  let u1 = diffuse(u0, 1, n_solver_steps, viscosity, time_step)
  let v1 = diffuse(v0, 2, n_solver_steps, viscosity, time_step)
  let (u2, v2) = project(n_solver_steps, u1, v1)
  let u3 = advect(u2, u2, v2, 1, time_step)
  let v3 = advect(v2, u2, v2, 2, time_step)
  let (u4, v4) = project(n_solver_steps, u3, v3)
  in (u4, v4)

let step(u0: [#g][#g]f32,
         v0: [#g][#g]f32,
         d0: [#g][#g]f32,
         n_solver_steps: i32,
         time_step: f32,
         diffusion_rate: f32,
         viscosity: f32): (*[g][g]f32,
                           *[g][g]f32,
                           *[g][g]f32) =
  let (u1, v1) = vel_step(u0, v0, n_solver_steps,
                          viscosity, time_step)
  let d1 = dens_step(d0, u0, v0, n_solver_steps,
                     diffusion_rate, time_step)
  in (u1, v1, d1)


------------------------------------------------------------
-- Wrapper functions.
------------------------------------------------------------

let get_end_frame(u0: [#g][#g]f32,
                  v0: [#g][#g]f32,
                  d0: [#g][#g]f32,
                  n_steps: i32,
                  n_solver_steps: i32,
                  time_step: f32,
                  diffusion_rate: f32,
                  viscosity: f32): ([g][g]f32,
                                    [g][g]f32,
                                    [g][g]f32) =
  loop (u0, v0, d0) for _i < n_steps do
    step(u0, v0, d0, n_solver_steps, time_step,
         diffusion_rate, viscosity)

let get_all_frames(u0: [#g][#g]f32,
                   v0: [#g][#g]f32,
                   d0: [#g][#g]f32,
                   n_steps: i32,
                   n_solver_steps: i32,
                   time_step: f32,
                   diffusion_rate: f32,
                   viscosity: f32): ([n_steps][g][g]f32,
                                     [n_steps][g][g]f32,
                                     [n_steps][g][g]f32) =
  let u_out = replicate n_steps u0
  let v_out = replicate n_steps v0
  let d_out = replicate n_steps d0
  in loop (u_out, v_out, d_out) for i in [1..<n_steps] do
    let (u0, v0, d0) = (u_out[i - 1], v_out[i - 1], d_out[i - 1])
    let (u1, v1, d1) = step(u0, v0, d0, n_solver_steps, time_step,
                            diffusion_rate, viscosity)
    let u_out[i] = u1
    let v_out[i] = v1
    let d_out[i] = d1
    in (u_out, v_out, d_out)

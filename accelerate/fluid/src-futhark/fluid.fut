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
-- ==
-- tags { disable }

------------------------------------------------------------
-- General helper functions.
------------------------------------------------------------

fun bool inside(i32 i, i32 j, i32 g) =
  i >= 1 && i <= g - 2
  && j >= 1 && j <= g - 2

fun bool in_outside_corner(i32 i, i32 j, i32 g) =
  (i == 0 || i == g - 1) && (j == 0 || j == g - 1)
  
fun (i32, i32, i32, i32) corner_index_neighbors(i32 i, i32 j, i32 g) =
  if i == 0 && j == 0
  then (1, 0, 0, 1)
  else if i == 0 && j == g - 1
  then (1, g - 1, 0, g - 2)
  else if i == g - 1 && j == 0
  then (g - 2, 0, g - 1, 1)
  else (g - 2, g - 1, g - 1, g - 2) -- if i == g - 1 && j == g - 1

fun (i32, i32, f32) outermost_inner_index(i32 i, i32 j, i32 g, i32 b) =
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

fun [g][g]f32
  lin_solve(i32 n_solver_steps,
            [g][g]f32 s0,
            i32 b,
            f32 a,
            f32 c) =
  loop (s1 = replicate(g, replicate(g, 0.0f32))) = for k < n_solver_steps do
    reshape((g, g),
      map(fn f32 (i32 ij) =>
            let i = ij / g in
            let j = ij % g in
            if inside(i, j, g)
            then lin_solve_inner(i, j, s0, s1, a, c)
            else lin_solve_outer(i, j, s0, s1, a, c, b)
         , iota(g * g)))
  in s1

fun f32
  lin_solve_inner(i32 i,
                  i32 j,
                  [g][g]f32 s0,
                  [g][g]f32 s1,
                  f32 a,
                  f32 c) =
  -- A stencil.
  unsafe ((s0[i, j] + a *
           (s1[(i - 1), j]
            + s1[(i + 1), j]
            + s1[i, (j - 1)]
            + s1[i, (j + 1)])) / c)

fun f32
  lin_solve_outer(i32 i,
                  i32 j,
                  [g][g]f32 s0,
                  [g][g]f32 s1,
                  f32 a,
                  f32 c,
                  i32 b) =
  if in_outside_corner(i, j, g)
  then let (i1, j1, i2, j2) = corner_index_neighbors(i, j, g)
       in 0.5f32 * (lin_solve_outer_base(i1, j1, s0, s1, a, c, b)
                    + lin_solve_outer_base(i2, j2, s0, s1, a, c, b))
  else lin_solve_outer_base(i, j, s0, s1, a, c, b)

fun f32
  lin_solve_outer_base(i32 i,
                       i32 j,
                       [g][g]f32 s0,
                       [g][g]f32 s1,
                       f32 a,
                       f32 c,
                       i32 b) =
  let (i1, j1, f) = outermost_inner_index(i, j, g, b)
  in f * lin_solve_inner(i1, j1, s0, s1, a, c)


------------------------------------------------------------
-- diffuse.
------------------------------------------------------------

fun [g][g]f32
  diffuse([g][g]f32 s,
          i32 b,
          i32 n_solver_steps,
          f32 diffusion_rate_or_viscosity,
          f32 time_step) =
  let a = (time_step * diffusion_rate_or_viscosity
           * f32(g - 2) * f32(g - 2)) in
  lin_solve(n_solver_steps, s, b, a, 1.0f32 + 4.0f32 * a)


------------------------------------------------------------
-- advect.
------------------------------------------------------------

fun *[g][g]f32
  advect([g][g]f32 s0,
         [g][g]f32 u,
         [g][g]f32 v,
         i32 b,
         f32 time_step) =
  let time_step0 = time_step * f32(g - 2) in
  reshape((g, g), 
    map(fn f32 (i32 ij) =>
          let i = ij / g in
          let j = ij % g in
          if inside(i, j, g)
          then advect_inner(i, j, s0, u, v, time_step0)
          else advect_outer(i, j, s0, u, v, time_step0, b)
       , iota(g * g)))

fun f32
  advect_inner(i32 i,
               i32 j,
               [g][g]f32 s,
               [g][g]f32 u,
               [g][g]f32 v,
               f32 time_step0) =
  let x = f32(i) - time_step0 * unsafe u[i, j] in
  let y = f32(j) - time_step0 * unsafe v[i, j] in

  let x = if x < 0.5f32 then 0.5f32 else x in
  let x = if x > f32(g - 2) + 0.5f32 then f32(g - 2) + 0.5f32 else x in
  let i0 = i32(x) in
  let i1 = i0 + 1 in

  let y = if y < 0.5f32 then 0.5f32 else y in
  let y = if y > f32(g - 2) + 0.5f32 then f32(g - 2) + 0.5f32 else y in
  let j0 = i32(y) in
  let j1 = j0 + 1 in

  let s1 = x - f32(i0) in
  let s0 = 1.0f32 - s1 in
  let t1 = y - f32(j0) in
  let t0 = 1.0f32 - t1 in

  unsafe (s0 * (t0 * s[i0, j0] + t1 * s[i0, j1])
          + s1 * (t0 * s[i1, j0] + t1 * s[i1, j1]))

fun f32
  advect_outer(i32 i,
               i32 j,
               [g][g]f32 s,
               [g][g]f32 u,
               [g][g]f32 v,
               f32 time_step0,
               i32 b) =
  if in_outside_corner(i, j, g)
  then let (i1, j1, i2, j2) = corner_index_neighbors(i, j, g)
       in 0.5f32 * (advect_outer_base(i1, j1, s, u, v, time_step0, b)
                    + advect_outer_base(i2, j2, s, u, v, time_step0, b))
  else advect_outer_base(i, j, s, u, v, time_step0, b)

fun f32
  advect_outer_base(i32 i,
                    i32 j,
                    [g][g]f32 s,
                    [g][g]f32 u,
                    [g][g]f32 v,
                    f32 time_step0,
                    i32 b) =
  let (i1, j1, f) = outermost_inner_index(i, j, g, b)
  in f * advect_inner(i1, j1, s, u, v, time_step0)


------------------------------------------------------------
-- project.
------------------------------------------------------------

fun (*[g][g]f32,
     *[g][g]f32)
  project(i32 n_solver_steps,
          [g][g]f32 u0,
          [g][g]f32 v0) =
  let div0 = project_top(u0, v0) in
  let p0 = lin_solve(n_solver_steps, div0, 0, 1.0f32, 4.0f32) in
  let u1 = project_bottom(p0, u0, 1, 1, 0, -1, 0) in
  let v1 = project_bottom(p0, v0, 2, 0, 1, 0, -1) in
  (u1, v1)

fun [g][g]f32
  project_top([g][g]f32 u0,
              [g][g]f32 v0) =
      reshape((g, g), 
        map(fn f32 (i32 ij) =>
              let i = ij / g in
              let j = ij % g in
              if inside(i, j, g)
              then project_top_inner(i, j, u0, v0)
              else project_top_outer(i, j, u0, v0)
           , iota(g * g)))

fun f32
  project_top_inner(i32 i,
                    i32 j,
                    [g][g]f32 u0,
                    [g][g]f32 v0) =
  unsafe (-0.5f32 * (u0[i + 1, j]
                     - u0[i - 1, j]
                     + v0[i, j + 1]
                     - v0[i, j - 1]) / f32(g))

fun f32
  project_top_outer(i32 i,
                    i32 j,
                    [g][g]f32 u0,
                    [g][g]f32 v0) =
  if in_outside_corner(i, j, g)
  then let (i1, j1, i2, j2) = corner_index_neighbors(i, j, g)
       in 0.5f32 * (project_top_outer_base(i1, j1, u0, v0)
                    + project_top_outer_base(i2, j2, u0, v0))
  else project_top_outer_base(i, j, u0, v0)

fun f32
  project_top_outer_base(i32 i,
                         i32 j,
                         [g][g]f32 u0,
                         [g][g]f32 v0) =
  let (i1, j1, f) = outermost_inner_index(i, j, g, 0)
  in project_top_inner(i1, j1, u0, v0)

fun *[g][g]f32
  project_bottom([g][g]f32 p0,
                 [g][g]f32 s0,
                 i32 b,
                 i32 i0d,
                 i32 j0d,
                 i32 i1d,
                 i32 j1d) =
      reshape((g, g), 
        map(fn f32 (i32 ij) =>
          let i = ij / g in
          let j = ij % g in
          if inside(i, j, g)
          then project_bottom_inner(i, j, p0, s0, i0d, j0d, i1d, j1d)
          else project_bottom_outer(i, j, p0, s0,
                                    i0d, j0d, i1d, j1d, b)
           , iota(g * g)))

fun f32
  project_bottom_inner(i32 i,
                       i32 j,
                       [g][g]f32 p0,
                       [g][g]f32 s0,
                       i32 i0d,
                       i32 j0d,
                       i32 i1d,
                       i32 j1d) =
  unsafe (s0[i, j] - 0.5f32 * f32(g - 2)
          * (p0[i + i0d, j + j0d] - p0[i + i1d, j + j1d]))

fun f32
  project_bottom_outer(i32 i,
                       i32 j,
                       [g][g]f32 p0,
                       [g][g]f32 s0,
                       i32 i0d,
                       i32 j0d,
                       i32 i1d,
                       i32 j1d,
                       i32 b) =
  if in_outside_corner(i, j, g)
  then let (i1, j1, i2, j2) = corner_index_neighbors(i, j, g)
       in 0.5f32 * (project_bottom_outer_base(i1, j1, p0, s0, i0d, j0d, i1d, j1d, b)
                    + project_bottom_outer_base(i2, j2, p0, s0, i0d, j0d, i1d, j1d, b))
  else project_bottom_outer_base(i, j, p0, s0, i0d, j0d, i1d, j1d, b)

fun f32
  project_bottom_outer_base(i32 i,
                            i32 j,
                            [g][g]f32 p0,
                            [g][g]f32 s0,
                            i32 i0d,
                            i32 j0d,
                            i32 i1d,
                            i32 j1d,
                            i32 b) =
  let (i1, j1, f) = outermost_inner_index(i, j, g, b)
  in f * project_bottom_inner(i1, j1, p0, s0, i0d, j0d, i1d, j1d)


------------------------------------------------------------
-- Step functions.
------------------------------------------------------------

fun *[g][g]f32
  dens_step([g][g]f32 d0,
            [g][g]f32 u0,
            [g][g]f32 v0,
            i32 n_solver_steps,
            f32 diffusion_rate,
            f32 time_step) =
  let d1 = diffuse(d0, 0, n_solver_steps, diffusion_rate, time_step) in
  let d2 = advect(d1, u0, v0, 0, time_step) in
  d2

fun (*[g][g]f32,
     *[g][g]f32)
  vel_step([g][g]f32 u0,
           [g][g]f32 v0,
           i32 n_solver_steps,
           f32 viscosity,
           f32 time_step) =
  let u1 = diffuse(u0, 1, n_solver_steps, viscosity, time_step) in
  let v1 = diffuse(v0, 2, n_solver_steps, viscosity, time_step) in
  let (u2, v2) = project(n_solver_steps, u1, v1) in
  let u3 = advect(u2, u2, v2, 1, time_step) in
  let v3 = advect(v2, u2, v2, 2, time_step) in
  let (u4, v4) = project(n_solver_steps, u3, v3) in
  (u4, v4)

fun (*[g][g]f32,
     *[g][g]f32,
     *[g][g]f32)
     step([g][g]f32 u0,
          [g][g]f32 v0,
          [g][g]f32 d0,
          i32 n_solver_steps,
          f32 time_step,
          f32 diffusion_rate,
          f32 viscosity) =
  let (u1, v1) = vel_step(u0, v0, n_solver_steps,
                          viscosity, time_step) in
  let d1 = dens_step(d0, u0, v0, n_solver_steps,
                     diffusion_rate, time_step) in
  (u1, v1, d1)


------------------------------------------------------------
-- Wrapper functions.
------------------------------------------------------------

fun ([g][g]f32,
     [g][g]f32,
     [g][g]f32)
  get_end_frame([g][g]f32 u0,
                [g][g]f32 v0,
                [g][g]f32 d0,
                i32 n_steps,
                i32 n_solver_steps,
                f32 time_step,
                f32 diffusion_rate,
                f32 viscosity) =
  loop ((u0, v0, d0)) = for i < n_steps do
    step(u0, v0, d0, n_solver_steps, time_step,
         diffusion_rate, viscosity)
  in (u0, v0, d0)

fun ([n_steps][g][g]f32,
     [n_steps][g][g]f32,
     [n_steps][g][g]f32)
  get_all_frames([g][g]f32 u0,
                 [g][g]f32 v0,
                 [g][g]f32 d0,
                 i32 n_steps,
                 i32 n_solver_steps,
                 f32 time_step,
                 f32 diffusion_rate,
                 f32 viscosity) =
  let u_out = replicate(n_steps, u0) in
  let v_out = replicate(n_steps, v0) in
  let d_out = replicate(n_steps, d0) in
  loop ((u_out, v_out, d_out)) = for 1 <= i < n_steps do
    let (u0, v0, d0) = (u_out[i - 1], v_out[i - 1], d_out[i - 1]) in
    let (u1, v1, d1) = step(u0, v0, d0, n_solver_steps, time_step,
                            diffusion_rate, viscosity) in
    let u_out[i] = u1 in
    let v_out[i] = v1 in
    let d_out[i] = d1 in
    (u_out, v_out, d_out)
  in (u_out, v_out, d_out)

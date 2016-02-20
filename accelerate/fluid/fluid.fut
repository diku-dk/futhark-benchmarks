-- Fluid simulation.
--
-- A port of Accelerate's version:
-- https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/fluid
-- (mostly based on the C version, since it was simpler).
--
-- This port stays true to the slightly weird indexing of the original C program
-- (at least for now) to avoid indexing-related bugs.
--
-- Variable naming conventions:
--
--   + `g`: The grid resolution.  The fluid simulation works on a square grid of
--          size `(g + 2) * (g + 2)`.
--   + `S*`: Some array, purpose unknown.
--   + `U*`: Array of horizontal forces.
--   + `V*`: Array of vertical forces.
--   + `D*`: Array of densities.
--
-- The simulation handles the 1-size border around the `g * g` grid differently
-- from the inner content.  The border depends on the outermost inner values.
-- The original C version writes this border after writing the inner values, by
-- reading from the array.  We would like to handle this pattern with a single
-- map, so the Futhark port instead *calculates* the outermost inner values when
-- it needs them for the outer bound values, which means that a few calculations
-- are done twice.  The alternative would be to first calculate all the inner
-- values, and then write the outer values afterwards.

------------------------------------------------------------
-- General functions.
------------------------------------------------------------

fun int n_elems_expected(int g) =
  (g + 2) * (g + 2)

fun int index(int i, int j, int g) =
  i + (g + 2) * j


------------------------------------------------------------
-- lin_solve.
------------------------------------------------------------

fun *[real, n_elems]
  lin_solve([real, n_elems] S0,
            int b,
            real a,
            real c,
            int g) =
  loop (S1 = replicate(n_elems, 0.0)) = for k < 20 do
    map(fn real (int k) =>
          let i = k % (g + 2) in
          let j = k / (g + 2) in
          if (i >= 1 && i <= g
              && j >= 1 && j <= g)
          then
            lin_solve_inner(i, j, S0, S1, a, c, g)
          else
            lin_solve_outer(i, j, S0, S1, a, c, g, b),
        iota(n_elems_expected(g)))
  in S1

fun real
  lin_solve_inner(int i,
                  int j,
                  [real, n_elems] S0,
                  [real, n_elems] S1,
                  real a,
                  real c,
                  int g) =
  -- A stencil.
  let middle = index(i, j, g) in
  let left = index(i - 1, j, g) in
  let right = index(i + 1, j, g) in
  let top = index(i, j - 1, g) in
  let bottom = index(i, j + 1, g) in
  (S0[middle] + a * (S1[left] + S1[right] + S1[top] + S1[bottom])) / c

fun real
  lin_solve_outer(int i,
                  int j,
                  [real, n_elems] S0,
                  [real, n_elems] S1,
                  real a,
                  real c,
                  int g,
                  int b) =
  if i == 0 && j == 0
  then 0.5 * (lin_solve_outer(1, 0, S0, S1, a, c, g, b)
              + lin_solve_outer(0, 1, S0, S1, a, c, g, b))
  else if i == 0 && j == g + 1
  then 0.5 * (lin_solve_outer(1, g + 1, S0, S1, a, c, g, b)
              + lin_solve_outer(0, g, S0, S1, a, c, g, b))
  else if i == g + 1 && j == 0
  then 0.5 * (lin_solve_outer(g, 0, S0, S1, a, c, g, b)
              + lin_solve_outer(g + 1, 1, S0, S1, a, c, g, b))
  else if i == g + 1 && j == g + 1
  then 0.5 * (lin_solve_outer(g, g + 1, S0, S1, a, c, g, b)
              + lin_solve_outer(g + 1, g, S0, S1, a, c, g, b))
  else if i == 0
  then if b == 1
       then -lin_solve_inner(1, j, S0, S1, a, c, g)
       else lin_solve_inner(1, j, S0, S1, a, c, g)
  else if i == g + 1
  then if b == 1
       then -lin_solve_inner(g, j, S0, S1, a, c, g)
       else lin_solve_inner(g, j, S0, S1, a, c, g)
  else if j == 0
  then if b == 2
       then -lin_solve_inner(i, 1, S0, S1, a, c, g)
       else lin_solve_inner(i, 1, S0, S1, a, c, g)
  else if j == g + 1
  then if b == 2
       then -lin_solve_inner(i, g, S0, S1, a, c, g)
       else lin_solve_inner(i, g, S0, S1, a, c, g)
  else 0.0 -- This is not supposed to happen.


------------------------------------------------------------
-- diffuse.
------------------------------------------------------------

fun *[real, n_elems]
  diffuse([real, n_elems] S,
          int b,
          int g,
          real diffusion_rate_or_viscosity,
          real time_step) =
  let a = (time_step * diffusion_rate_or_viscosity
           * real(g) * real(g)) in
  lin_solve(S, b, a, 1.0 + 4.0 * a, g)


------------------------------------------------------------
-- advect.
------------------------------------------------------------

fun *[real, n_elems]
  advect([real, n_elems] S0,
         [real, n_elems] U,
         [real, n_elems] V,
         int b,
         int g,
         real time_step) =
  let time_step0 = time_step * real(g) in
    map(fn real (int k) =>
          let i = k % (g + 2) in
          let j = k / (g + 2) in
          if (i >= 1 && i <= g
              && j >= 1 && j <= g)
          then
            advect_inner(i, j, S0, U, V, g, time_step0)
          else
            advect_outer(i, j, S0, U, V, g, time_step0, b),
        iota(n_elems_expected(g)))

fun real
  advect_inner(int i,
               int j,
               [real, n_elems] S0,
               [real, n_elems] U,
               [real, n_elems] V,
               int g,
               real time_step0) =
  let x = real(i) - time_step0 * U[index(i, j, g)] in
  let y = real(j) - time_step0 * V[index(i, j, g)] in

  let x = if x < 0.5 then 0.5 else x in
  let x = if x > real(g) + 0.5 then real(g) + 0.5 else x in
  let i0 = int(x) in
  let i1 = i0 + 1 in

  let y = if y < 0.5 then 0.5 else y in
  let y = if y > real(g) + 0.5 then real(g) + 0.5 else y in
  let j0 = int(y) in
  let j1 = j0 + 1 in

  let s1 = x - real(i0) in
  let s0 = 1.0 - s1 in
  let t1 = y - real(j0) in
  let t0 = 1.0 - t1 in

  (s0 * (t0 * S0[index(i0, j0, g)] + t1 * S0[index(i0, j1, g)])
   + s1 * (t0 * S0[index(i1, j0, g)] + t1 * S0[index(i1, j1, g)]))

fun real
  advect_outer(int i,
               int j,
               [real, n_elems] S0,
               [real, n_elems] U,
               [real, n_elems] V,
               int g,
               real time_step0,
               int b) =
  if i == 0 && j == 0
  then 0.5 * (advect_outer(1, 0, S0, U, V, g, time_step0, b)
              + advect_outer(0, 1, S0, U, V, g, time_step0, b))
  else if i == 0 && j == g + 1
  then 0.5 * (advect_outer(1, g + 1, S0, U, V, g, time_step0, b)
              + advect_outer(0, g, S0, U, V, g, time_step0, b))
  else if i == g + 1 && j == 0
  then 0.5 * (advect_outer(g, 0, S0, U, V, g, time_step0, b)
              + advect_outer(g + 1, 1, S0, U, V, g, time_step0, b))
  else if i == g + 1 && j == g + 1
  then 0.5 * (advect_outer(g, g + 1, S0, U, V, g, time_step0, b)
              + advect_outer(g + 1, g, S0, U, V, g, time_step0, b))
  else if i == 0
  then if b == 1
       then -advect_inner(1, j, S0, U, V, g, time_step0)
       else advect_inner(1, j, S0, U, V, g, time_step0)
  else if i == g + 1
  then if b == 1
       then -advect_inner(g, j, S0, U, V, g, time_step0)
       else advect_inner(g, j, S0, U, V, g, time_step0)
  else if j == 0
  then if b == 2
       then -advect_inner(i, 1, S0, U, V, g, time_step0)
       else advect_inner(i, 1, S0, U, V, g, time_step0)
  else if j == g + 1
  then if b == 2
       then -advect_inner(i, g, S0, U, V, g, time_step0)
       else advect_inner(i, g, S0, U, V, g, time_step0)
  else 0.0 -- This is not supposed to happen.


------------------------------------------------------------
-- project.
------------------------------------------------------------

fun {*[real, n_elems],
     *[real, n_elems]}
  project([real, n_elems] U0,
          [real, n_elems] V0,
          int g) =
  let Div0 = project_top(U0, V0, g) in
  let P0 = lin_solve(Div0, 0, 1.0, 4.0, g) in
  let U1 = project_bottom(P0, U0, 1, 1, 0, -1, 0, g) in
  let V1 = project_bottom(P0, V0, 2, 0, 1, 0, -1, g) in
  {U1, V1}

fun *[real, n_elems]
  project_top([real, n_elems] U0,
              [real, n_elems] V0,
              int g) =
  map(fn real (int k) =>
        let i = k % (g + 2) in
        let j = k / (g + 2) in
        if (i >= 1 && i <= g
            && j >= 1 && j <= g)
        then
          project_top_inner(i, j, U0, V0, g)
        else
          project_top_outer(i, j, U0, V0, g),
      iota(n_elems_expected(g)))

fun real
  project_top_inner(int i,
                    int j,
                    [real, n_elems] U0,
                    [real, n_elems] V0,
                    int g) =
  (-0.5 * (U0[index(i + 1, j, g)]
           - U0[index(i - 1, j, g)]
           + V0[index(i, j + 1, g)]
           - V0[index(i, j - 1, g)]) / real(g))

fun real
  project_top_outer(int i,
                    int j,
                    [real, n_elems] U0,
                    [real, n_elems] V0,
                    int g) =
  if i == 0 && j == 0
  then 0.5 * (project_top_outer(1, 0, U0, V0, g)
              + project_top_outer(0, 1, U0, V0, g))
  else if i == 0 && j == g + 1
  then 0.5 * (project_top_outer(1, g + 1, U0, V0, g)
              + project_top_outer(0, g, U0, V0, g))
  else if i == g + 1 && j == 0
  then 0.5 * (project_top_outer(g, 0, U0, V0, g)
              + project_top_outer(g + 1, 1, U0, V0, g))
  else if i == g + 1 && j == g + 1
  then 0.5 * (project_top_outer(g, g + 1, U0, V0, g)
              + project_top_outer(g + 1, g, U0, V0, g))
  else if i == 0
  then project_top_inner(1, j, U0, V0, g)
  else if i == g + 1
  then project_top_inner(g, j, U0, V0, g)
  else if j == 0
  then project_top_inner(i, 1, U0, V0, g)
  else if j == g + 1
  then project_top_inner(i, g, U0, V0, g)
  else 0.0 -- This is not supposed to happen.

fun *[real, n_elems]
  project_bottom([real, n_elems] P0,
                 [real, n_elems] S0,
                 int b,
                 int i0d,
                 int j0d,
                 int i1d,
                 int j1d,
                 int g) =
  map(fn real (int k) =>
        let i = k % (g + 2) in
        let j = k / (g + 2) in
        if (i >= 1 && i <= g
            && j >= 1 && j <= g)
        then
          project_bottom_inner(i, j, P0, S0, i0d, j0d, i1d, j1d, g)
        else
          project_bottom_outer(i, j, P0, S0, i0d, j0d, i1d, j1d, g, b),
        iota(n_elems_expected(g)))

fun real
  project_bottom_inner(int i,
                       int j,
                       [real, n_elems] P0,
                       [real, n_elems] S0,
                       int i0d,
                       int j0d,
                       int i1d,
                       int j1d,
                       int g) =
  (S0[index(i, j, g)] - 0.5 * real(g)
   * (P0[index(i + i0d, j + j0d, g)] - P0[index(i + i1d, j + j1d, g)]))

fun real
  project_bottom_outer(int i,
                       int j,
                       [real, n_elems] P0,
                       [real, n_elems] S0,
                       int i0d,
                       int j0d,
                       int i1d,
                       int j1d,
                       int g,
                       int b) =
  if i == 0 && j == 0
  then 0.5 * (project_bottom_outer(1, 0, P0, S0, i0d, j0d, i1d, j1d, g, b)
              + project_bottom_outer(0, 1, P0, S0, i0d, j0d, i1d, j1d, g, b))
  else if i == 0 && j == g + 1
  then 0.5 * (project_bottom_outer(1, g + 1, P0, S0, i0d, j0d, i1d, j1d, g, b)
              + project_bottom_outer(0, g, P0, S0, i0d, j0d, i1d, j1d, g, b))
  else if i == g + 1 && j == 0
  then 0.5 * (project_bottom_outer(g, 0, P0, S0, i0d, j0d, i1d, j1d, g, b)
              + project_bottom_outer(g + 1, 1, P0, S0, i0d, j0d, i1d, j1d, g, b))
  else if i == g + 1 && j == g + 1
  then 0.5 * (project_bottom_outer(g, g + 1, P0, S0, i0d, j0d, i1d, j1d, g, b)
              + project_bottom_outer(g + 1, g, P0, S0, i0d, j0d, i1d, j1d, g, b))
  else if i == 0
  then if b == 1
       then -project_bottom_inner(1, j, P0, S0, i0d, j0d, i1d, j1d, g)
       else project_bottom_inner(1, j, P0, S0, i0d, j0d, i1d, j1d, g)
  else if i == g + 1
  then if b == 1
       then -project_bottom_inner(g, j, P0, S0, i0d, j0d, i1d, j1d, g)
       else project_bottom_inner(g, j, P0, S0, i0d, j0d, i1d, j1d, g)
  else if j == 0
  then if b == 2
       then -project_bottom_inner(i, 1, P0, S0, i0d, j0d, i1d, j1d, g)
       else project_bottom_inner(i, 1, P0, S0, i0d, j0d, i1d, j1d, g)
  else if j == g + 1
  then if b == 2
       then -project_bottom_inner(i, g, P0, S0, i0d, j0d, i1d, j1d, g)
       else project_bottom_inner(i, g, P0, S0, i0d, j0d, i1d, j1d, g)
  else 0.0 -- This is not supposed to happen.


------------------------------------------------------------
-- Step functions.
------------------------------------------------------------

fun *[real, n_elems]
  dens_step([real, n_elems] D0,
            [real, n_elems] U0,
            [real, n_elems] V0,
            int g,
            real diffusion_rate,
            real time_step) =
  let D1 = diffuse(D0, 0, g, diffusion_rate, time_step) in
  let D2 = advect(D1, U0, V0, 0, g, time_step) in
  D2

fun {*[real, n_elems],
     *[real, n_elems]}
  vel_step([real, n_elems] U0,
           [real, n_elems] V0,
           int g,
           real viscosity,
           real time_step) =
  let U1 = diffuse(U0, 1, g, viscosity, time_step) in
  let V1 = diffuse(V0, 2, g, viscosity, time_step) in
  let {U2, V2} = project(U1, V1, g) in
  let U3 = advect(U2, U2, V2, 1, g, time_step) in
  let V3 = advect(V2, U2, V2, 2, g, time_step) in
  let {U4, V4} = project(U3, V3, g) in
  {U4, V4}

fun {*[real, n_elems],
     *[real, n_elems],
     *[real, n_elems]}
     step([real, n_elems] U0,
          [real, n_elems] V0,
          [real, n_elems] D0,
          int g,
          real time_step,
          real diffusion_rate,
          real viscosity) =
  let {U1, V1} = vel_step(U0, V0, g, viscosity, time_step) in
  let D1 = dens_step(D0, U0, V0, g, diffusion_rate, time_step) in
  {U1, V1, D1}


------------------------------------------------------------
-- Wrapper functions.
------------------------------------------------------------

fun [[int, g], g]
  draw_densities([real] D,
                 int g) =
  let ks = map(fn int (int k) => k + 1, iota(g)) in
  map(fn [int, g] (int i) =>
        map (fn int (int j) =>
               int(255.0 * D[index(i, j, g)]), ks),
        ks)

fun {[real, n_elems],
     [real, n_elems],
     [real, n_elems]}
  get_end_frame([real, n_elems] U0,
                [real, n_elems] V0,
                [real, n_elems] D0,
                int n_steps,
                int g,
                real time_step,
                real diffusion_rate,
                real viscosity) =
  if ! (n_elems == n_elems_expected(g))
  then {U0, V0, D0} -- This should really be an error.
  else
    loop ({U0, V0, D0}) = for 1 <= i < n_steps do
      step(U0, V0, D0, g, time_step,
           diffusion_rate, viscosity)
    in {U0, V0, D0}

fun [[int, g], g]
  draw_end_frame([real, n_elems] U0,
                 [real, n_elems] V0,
                 [real, n_elems] D0,
                 int n_steps,
                 int g,
                 real time_step,
                 real diffusion_rate,
                 real viscosity) =
  let {U, V, D} = get_end_frame(U0, V0, D0, n_steps, g,
                                time_step, diffusion_rate, viscosity) in
  draw_densities(D, g)

fun {[[real, n_elems], n_steps],
     [[real, n_elems], n_steps],
     [[real, n_elems], n_steps]}
  get_all_frames([real, n_elems] U0,
                 [real, n_elems] V0,
                 [real, n_elems] D0,
                 int n_steps,
                 int g,
                 real time_step,
                 real diffusion_rate,
                 real viscosity) =
  let U_out = replicate(n_steps, U0) in
  let V_out = replicate(n_steps, V0) in
  let D_out = replicate(n_steps, D0) in
  if ! (n_elems == n_elems_expected(g))
  then {U_out, V_out, D_out} -- This should really be an error.
  else
    loop ({U_out, V_out, D_out}) = for 1 <= i < n_steps do
      let {U0, V0, D0} = {U_out[i - 1], V_out[i - 1], D_out[i - 1]} in
      let {U1, V1, D1} = step(U0, V0, D0, g, time_step,
                              diffusion_rate, viscosity) in
      let U_out[i] = U1 in
      let V_out[i] = V1 in
      let D_out[i] = D1 in
      {U_out, V_out, D_out}
    in {U_out, V_out, D_out}

fun [[[int, g], g], n_steps]
  draw_all_frames([real, n_elems] U0,
                  [real, n_elems] V0,
                  [real, n_elems] D0,
                  int n_steps,
                  int g,
                  real time_step,
                  real diffusion_rate,
                  real viscosity) =
  let {Us, Vs, Ds} = get_all_frames(U0, V0, D0, n_steps, g,
                                    time_step, diffusion_rate, viscosity) in
  map(fn [[int, g], g]
        ([real, n_elems] D) => draw_densities(D, g), Ds)


------------------------------------------------------------
-- main.
------------------------------------------------------------

fun [[[int, g], g]]
  main([real, n_elems] U0,
       [real, n_elems] V0,
       [real, n_elems] D0,
       int n_steps,
       int g,
       real time_step,
       real diffusion_rate,
       real viscosity) =
  draw_all_frames(U0, V0, D0, n_steps, g,
                  time_step, diffusion_rate, viscosity)
  -- [draw_end_frame(U0, V0, D0, n_steps, g,
  --                 time_step, diffusion_rate, viscosity)]

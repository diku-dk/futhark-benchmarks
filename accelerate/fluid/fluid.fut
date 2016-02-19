-- Fluid simulation.
--
-- A port of Accelerate's version:
-- https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/fluid
-- (mostly based on the C version, since it was simpler).
--
-- This port stays true to the slightly weird indexing of the original C program
-- (at least for now) to avoid indexing-related bugs.
--
-- This port does not perform quite like the original; a few bugs will have to
-- be ironed out.

fun int n_elems_expected(int grid_resolution) =
  (grid_resolution + 2) * (grid_resolution + 2)

fun int index(int i, int j, int grid_resolution) =
  i + (grid_resolution + 2) * j

fun *[real, n_elems]
  set_bnd(*[real, n_elems] S,
          int b,
          int grid_resolution) =
  -- This is probably parallelizable, but it's difficult with the existing
  -- indexing scheme.
  let g = grid_resolution in
  loop (S) = for 1 <= i < grid_resolution + 1 do
    let S[index(0, i, g)] =
      if b == 1
      then -S[index(1, i, g)]
      else S[index(1, i, g)] in
    let S[index(g + 1, i, g)] =
      if b == 1
      then -S[index(g, i, g)]
      else S[index(g, i, g)] in
    let S[index(i, 0, g)] =
      if b == 2
      then -S[index(i, 1, g)]
      else S[index(i, 1, g)] in
    let S[index(i, g + 1, g)] =
      if b == 2
      then -S[index(i, g, g)]
      else S[index(i, g, g)] in
    S in
  let S[index(0, 0, g)] = 0.5 * (S[index(1, 0, g)]
                                 + S[index(0, 1, g)]) in
  let S[index(0, g + 1, g)] = 0.5 * (S[index(1, g + 1, g)]
                                     + S[index(0, g, g)]) in
  let S[index(g + 1, 0, g)] = 0.5 * (S[index(g, 0, g)] +
                                     S[index(g + 1, 1, g)]) in
  let S[index(g + 1, g + 1, g)] = 0.5 * (S[index(g, g + 1, g)]
                                         + S[index(g + 1, g, g)]) in
  S
  
-- A stencil.
fun *[real, n_elems]
  lin_solve([real, n_elems] S0,
            [real, n_elems] S,
            int b,
            real a,
            real c,
            int grid_resolution) =
  -- Not quite as it is in solver.c, which uses garbage x (here S00) in its
  -- first run (here set to S0).  The thing in solver.c is probably a bug.
  loop (S00 = copy(S)) = for k < 20 do
    let S01 = for_each_cell_lin_solve(S0, S00, a, c, grid_resolution) in
    let S02 = set_bnd(S01, b, grid_resolution) in
    S02
  in S00

fun *[real, n_elems]
  for_each_cell_lin_solve([real, n_elems] S0,
                          [real, n_elems] S,
                          real a,
                          real c,
                          int grid_resolution) =
    map(fn real (int k) =>
          let i = k / (grid_resolution + 2) in
          let j = k % (grid_resolution + 2) in
          -- This, and the other places where this pattern occurs, should go
          -- away.  The problem is that the original implementation depends on a
          -- "frame" of width 1 in all corners (see `n_elems_expected'), but it
          -- can probably be circumvented.
          if (i == 0 || i == grid_resolution + 1 ||
              j == 0 || j == grid_resolution + 1)
          then -- Keep the old value for now.
            S0[index(i, j, grid_resolution)] 
          else -- Find the new value.
            let middle = index(i, j, grid_resolution) in
            let left = index(i - 1, j, grid_resolution) in
            let right = index(i + 1, j, grid_resolution) in
            let top = index(i, j - 1, grid_resolution) in
            let bottom = index(i, j + 1, grid_resolution) in
            (S0[middle] + a * (S[left] + S[right] + S[top] + S[bottom]))
            / c,
        iota(n_elems_expected(grid_resolution)))
  
fun *[real, n_elems]
  diffuse([real, n_elems] S,
          int b,
          int grid_resolution,
          real diffusion_rate_or_viscosity,
          real time_step) =
  let a = (time_step * diffusion_rate_or_viscosity
           * real(grid_resolution) * real(grid_resolution)) in
  lin_solve(S, S, b, a, 1.0 + 4.0 * a, grid_resolution)

fun *[real, n_elems]
  advect([real, n_elems] S0,
         [real, n_elems] U,
         [real, n_elems] V,
         int b,
         int grid_resolution,
         real time_step) =
  let time_step0 = time_step * real(grid_resolution) in
  let S1 = for_each_cell_advect(S0, U, V, time_step0, grid_resolution) in
  let S2 = set_bnd(S1, b, grid_resolution) in
  S2

fun *[real, n_elems]
  for_each_cell_advect([real, n_elems] S0,
                       [real, n_elems] U,
                       [real, n_elems] V,
                       real time_step0,
                       int grid_resolution) =
    let g = grid_resolution in
    map(fn real (int k) =>
          let i = k / (grid_resolution + 2) in
          let j = k % (grid_resolution + 2) in
          if (i == 0 || i == grid_resolution + 1 ||
              j == 0 || j == grid_resolution + 1)
          then -- Keep the old value for now.
            S0[index(i, j, grid_resolution)] 
          else -- Find the new value.
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
             + s1 * (t0 * S0[index(i1, j0, g)] + t1 * S0[index(i1, j1, g)])),
        iota(n_elems_expected(grid_resolution)))

fun {*[real, n_elems],
     *[real, n_elems]}
  project([real, n_elems] U0,
          [real, n_elems] V0,
          int grid_resolution) =
  let P0 = replicate(n_elems, 0.0) in
  let Div0 = for_each_cell_project_top(U0, V0, grid_resolution) in
  let Div1 = set_bnd(Div0, 0, grid_resolution) in
  let P1 = set_bnd(P0, 0, grid_resolution) in
  let P2 = lin_solve(Div1, P1, 0, 1.0, 4.0, grid_resolution) in
  let U1 = for_each_cell_project_bottom_u(P2, U0, grid_resolution) in
  let V1 = for_each_cell_project_bottom_v(P2, V0, grid_resolution) in
  let U2 = set_bnd(U1, 1, grid_resolution) in
  let V2 = set_bnd(V1, 2, grid_resolution) in
  {U2, V2}

fun *[real, n_elems]
  for_each_cell_project_top([real, n_elems] U0,
                            [real, n_elems] V0,
                            int grid_resolution) =
    let g = grid_resolution in
    map(fn real (int k) =>
          let i = k / (grid_resolution + 2) in
          let j = k % (grid_resolution + 2) in
          if (i == 0 || i == grid_resolution + 1 ||
              j == 0 || j == grid_resolution + 1)
          then -- Keep an old value for now.
            U0[index(i, j, grid_resolution)] 
          else -- Find the new value.
            (-0.5 * (U0[index(i + 1, j, g)]
                     - U0[index(i - 1, j, g)]
                     + V0[index(i, j + 1, g)]
                     - V0[index(i, j - 1, g)]) / real(g)),
        iota(n_elems_expected(grid_resolution)))

fun *[real, n_elems]
  for_each_cell_project_bottom_u([real, n_elems] P0,
                                 [real, n_elems] U0,
                                 int grid_resolution) =
    let g = grid_resolution in
    map(fn real (int k) =>
          let i = k / (grid_resolution + 2) in
          let j = k % (grid_resolution + 2) in
          if (i == 0 || i == grid_resolution + 1 ||
              j == 0 || j == grid_resolution + 1)
          then -- Keep the old value for now.
            P0[index(i, j, grid_resolution)] 
          else -- Find the new value.
            (U0[index(i, j, g)] - 0.5 * real(g)
             * (P0[index(i + 1, j, g)] - P0[index(i - 1, j, g)])),
        iota(n_elems_expected(grid_resolution)))

fun *[real, n_elems]
  for_each_cell_project_bottom_v([real, n_elems] P0,
                                 [real, n_elems] V0,
                                 int grid_resolution) =
    let g = grid_resolution in
    map(fn real (int k) =>
          let i = k / (grid_resolution + 2) in
          let j = k % (grid_resolution + 2) in
          if (i == 0 || i == grid_resolution + 1 ||
              j == 0 || j == grid_resolution + 1)
          then -- Keep the old value for now.
            P0[index(i, j, grid_resolution)] 
          else -- Find the new value.
            (V0[index(i, j, g)] - 0.5 * real(g)
             * (P0[index(i, j + 1, g)] - P0[index(i, j - 1, g)])),
        iota(n_elems_expected(grid_resolution)))

fun *[real, n_elems]
  dens_step([real, n_elems] D0,
            [real, n_elems] U0,
            [real, n_elems] V0,
            int grid_resolution,
            real diffusion_rate,
            real time_step) =
  let D1 = D0 in
  let D2 = diffuse(D1, 0, grid_resolution, diffusion_rate, time_step) in
  let D3 = advect(D2, U0, V0, 0, grid_resolution, time_step) in
  D3
                                                                 
fun {*[real, n_elems],
     *[real, n_elems]}
  vel_step([real, n_elems] U0,
           [real, n_elems] V0,
           int grid_resolution,
           real viscosity,
           real time_step) =
  let U1 = U0 in
  let V1 = V0 in
  let U2 = diffuse(U1, 1, grid_resolution, viscosity, time_step) in
  let V2 = diffuse(V1, 2, grid_resolution, viscosity, time_step) in
  let {U3, V3} = project(U2, V2, grid_resolution) in
  let U4 = advect(U3, U3, V3, 1, grid_resolution, time_step) in
  let V4 = advect(V3, U3, V3, 2, grid_resolution, time_step) in
  let {U5, V5} = project(U4, V4, grid_resolution) in
  {U5, V5}

fun {*[real, n_elems],
     *[real, n_elems],
     *[real, n_elems]}
     step([real, n_elems] U0,
          [real, n_elems] V0,
          [real, n_elems] D0,
          int grid_resolution,
          real time_step,
          real diffusion_rate,
          real viscosity) =
  let {U1, V1} = vel_step(U0, V0, grid_resolution, viscosity, time_step) in
  let D1 = dens_step(D0, U0, V0, grid_resolution, diffusion_rate, time_step) in
  {U1, V1, D1}

fun [[int, grid_resolution], grid_resolution]
  draw_densities([real] D,
                 int grid_resolution) =
  let ks = map(fn int (int k) => k + 1, iota(grid_resolution)) in
  map(fn [int, grid_resolution] (int i) =>
        map (fn int (int j) =>
               int(255.0 * D[index(i, j, grid_resolution)]), ks),
        ks)
  
fun {[real, n_elems],
     [real, n_elems],
     [real, n_elems]}
  get_end_frame([real, n_elems] U0,
                [real, n_elems] V0,
                [real, n_elems] D0,
                int n_steps,
                int grid_resolution,
                real time_step,
                real diffusion_rate,
                real viscosity) =
  if ! (n_elems == n_elems_expected(grid_resolution))
  then {U0, V0, D0} -- This should really be an error.
  else
    loop ({U0, V0, D0}) = for 1 <= i < n_steps do
      step(U0, V0, D0, grid_resolution, time_step,
           diffusion_rate, viscosity)
    in {U0, V0, D0}

fun [[int, grid_resolution], grid_resolution]
  draw_end_frame([real, n_elems] U0,
                 [real, n_elems] V0,
                 [real, n_elems] D0,
                 int n_steps,
                 int grid_resolution,
                 real time_step,
                 real diffusion_rate,
                 real viscosity) =
  let {U, V, D} = get_end_frame(U0, V0, D0, n_steps, grid_resolution,
                                time_step, diffusion_rate, viscosity) in
  draw_densities(D, grid_resolution)

fun {[[real, n_elems], n_steps],
     [[real, n_elems], n_steps],
     [[real, n_elems], n_steps]}
  get_all_frames([real, n_elems] U0,
                 [real, n_elems] V0,
                 [real, n_elems] D0,
                 int n_steps,
                 int grid_resolution,
                 real time_step,
                 real diffusion_rate,
                 real viscosity) =
  let U_out = replicate(n_steps, U0) in
  let V_out = replicate(n_steps, V0) in
  let D_out = replicate(n_steps, D0) in
  if ! (n_elems == n_elems_expected(grid_resolution))
  then {U_out, V_out, D_out} -- This should really be an error.
  else
    loop ({U_out, V_out, D_out}) = for 1 <= i < n_steps do
      let {U0, V0, D0} = {U_out[i - 1], V_out[i - 1], D_out[i - 1]} in
      let {U1, V1, D1} = step(U0, V0, D0, grid_resolution, time_step,
                              diffusion_rate, viscosity) in
      let U_out[i] = U1 in
      let V_out[i] = V1 in
      let D_out[i] = D1 in
      {U_out, V_out, D_out}
    in {U_out, V_out, D_out}

fun [[[int, grid_resolution], grid_resolution], n_steps]
  draw_all_frames([real, n_elems] U0,
                  [real, n_elems] V0,
                  [real, n_elems] D0,
                  int n_steps,
                  int grid_resolution,
                  real time_step,
                  real diffusion_rate,
                  real viscosity) =
  let {Us, Vs, Ds} = get_all_frames(U0, V0, D0, n_steps, grid_resolution,
                                    time_step, diffusion_rate, viscosity) in
  map(fn [[int, grid_resolution], grid_resolution]
        ([real, n_elems] D) => draw_densities(D, grid_resolution), Ds)

fun [[[int, grid_resolution], grid_resolution]]
  main([real, n_elems] U0,
       [real, n_elems] V0,
       [real, n_elems] D0,
       int n_steps,
       int grid_resolution,
       real time_step,
       real diffusion_rate,
       real viscosity) =
  draw_all_frames(U0, V0, D0, n_steps, grid_resolution,
                  time_step, diffusion_rate, viscosity)
  -- [draw_end_frame(U0, V0, D0, n_steps, grid_resolution,
  --                 time_step, diffusion_rate, viscosity)]

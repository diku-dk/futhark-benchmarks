-- Only implements event-based simulation.  The datasets use the
-- default "input" settings.
-- ==
-- input @ data/small.in.gz
-- output { 945990i64 }
-- no_rtx2080 no_k40 no_gtx780 input @ data/large.in.gz
-- output { 952131i64 }

type nuclide_grid_point =
  { energy: f64,
    total_xs: f64,
    elastic_xs: f64,
    absorbtion_xs: f64,
    fission_xs: f64,
    nu_fission_xs: f64
  }

type~ simulation_data [length_num_nucs]
                      [length_unionized_energy_array]
                      [max_num_nucs]
                      [n_isotopes]
                      [n_gridpoints] =
  { num_nucs: [length_num_nucs]i32,
    concs: [length_num_nucs][max_num_nucs]f64,
    mats: [length_num_nucs][max_num_nucs]i32,
    nuclide_grid: [n_isotopes][n_gridpoints]nuclide_grid_point,
    index_grid: [length_unionized_energy_array][n_isotopes]i32,
    unionized_energy_array: [length_unionized_energy_array]f64
  }

type grid_type = #unionized | #nuclide | #hash

type inputs =
  { n_isotopes: i64,
    n_gridpoints: i64,
    grid_type: grid_type,
    hash_bins: i64,
    lookups: i64
  }

let grid_search [n] (quarry: f64) (A: [n]f64) : i64 =
  let lowerLimit = 0
  let upperLimit = n-1
  in (.0) <| loop (lowerLimit, upperLimit) while upperLimit - lowerLimit > 1 do
             let examinationPoint = lowerLimit + ( (upperLimit - lowerLimit) / 2 )
             in if A[examinationPoint] > quarry
		   then (lowerLimit, examinationPoint)
		   else (examinationPoint, upperLimit)

let grid_search_nuclide [n] (quarry: f64) (A: [n]nuclide_grid_point) (low: i64) (high: i64) : i64 =
  let lowerLimit = low
  let upperLimit = high
  in (.0) <| loop (lowerLimit, upperLimit) while upperLimit - lowerLimit > 1 do
             let examinationPoint = lowerLimit + ( (upperLimit - lowerLimit) / 2 )
             in if A[examinationPoint].energy > quarry
		   then (lowerLimit, examinationPoint)
		   else (examinationPoint, upperLimit)

type xs_vector = (f64,f64,f64,f64,f64)

let calculate_micro_xs [n_isotopes] [n_gridpoints]
                       (p_energy: f64) (nuc: i32)
                       (index_data: [][n_isotopes]i32)
                       (nuclide_grids: [][n_gridpoints]nuclide_grid_point)
                       (idx: i64) (grid_type: grid_type) (hash_bins: i64) : xs_vector =
  let low_idx =
    match grid_type
    case #nuclide ->
      let idx = grid_search_nuclide p_energy nuclide_grids[nuc] 0 (n_gridpoints-1)
      in if idx == n_gridpoints-1
         then idx-1
         else idx
    case #unionized ->
      if i64.i32 (index_data[idx,nuc]) == n_gridpoints - 1
      then i64.i32 index_data[idx,nuc]-1
      else i64.i32 index_data[idx,nuc]
    case #hash ->
      let u_low = index_data[idx,nuc]
      let u_high = if idx == hash_bins - 1
                   then i32.i64 n_gridpoints - 1
                   else index_data[idx+1,nuc]
      let e_low = nuclide_grids[nuc,u_low].energy
      let e_high = nuclide_grids[nuc,u_high].energy
      let lower = if p_energy <= e_low
                  then 0
                  else if p_energy >= e_high
                  then n_gridpoints - 1
                  else grid_search_nuclide p_energy nuclide_grids[nuc] (i64.i32 u_low) (i64.i32 u_high)
      in if lower == n_gridpoints - 1
         then lower-1
         else lower
  let high_idx = low_idx + 1
  let high = nuclide_grids[nuc,high_idx]
  let low = nuclide_grids[nuc,low_idx]
  let f = (high.energy - p_energy) / (high.energy - low.energy)
  in (high.total_xs - f * (high.total_xs - low.total_xs),
      high.elastic_xs - f * (high.elastic_xs - low.elastic_xs),
      high.absorbtion_xs - f * (high.absorbtion_xs - low.absorbtion_xs),
      high.fission_xs - f * (high.fission_xs - low.fission_xs),
      high.nu_fission_xs - f * (high.nu_fission_xs - low.nu_fission_xs))

let calculate_macro_xs [n_isotopes] [n_gridpoints] [max_num_nucs]
                       (p_energy: f64) (mat: i32)
                       (num_nucs: []i32)
                       (concs: [][max_num_nucs]f64)
                       (egrid: []f64) (index_data: [][n_isotopes]i32)
                       (nuclide_grids: [][n_gridpoints]nuclide_grid_point)
                       (mats: [][max_num_nucs]i32)
                       (grid_type: grid_type) (hash_bins: i64) : xs_vector =
  let idx =
    match grid_type
    case #unionized ->
      grid_search p_energy egrid
    case #hash ->
      let du = 1/f64.i64 hash_bins
      in i64.f64 (p_energy / du)
    case #nuclide ->
      -1

  in loop macro_xs_vector = (0,0,0,0,0) for j < num_nucs[mat] do
     let p_nuc = mats[mat,j]
     let conc = concs[mat,j]
     let xs_vector = calculate_micro_xs p_energy p_nuc index_data nuclide_grids idx grid_type hash_bins
     in (macro_xs_vector.0 + xs_vector.0 * conc,
         macro_xs_vector.1 + xs_vector.1 * conc,
         macro_xs_vector.2 + xs_vector.2 * conc,
         macro_xs_vector.3 + xs_vector.3 * conc,
         macro_xs_vector.4 + xs_vector.4 * conc)

type seed = u64

let STARTING_SEED : seed = 1070

let fast_forward_LCG (seed: seed) (n: i64) : seed =
  let m = 9223372036854775808 : u64
  let a = 2806196910506780709 : u64
  let c = 1 : u64
  let n = u64.i64 n % m
  let (_, _, _, a_new, c_new) =
    loop (n, a, c, a_new, c_new) = (n, a, c, 1, 0) while n > 0 do
    let (a_new, c_new) =
      if n & 1 == 1
      then (a_new * a, c_new * a + c)
      else (a_new, c_new)
    in (n>>1, a * a, c * (a+1), a_new, c_new)
  in (a_new * seed + c_new) % m

let LCG_random_double (seed: seed) : (f64, seed) =
  let m = 9223372036854775808
  let a = 2806196910506780709
  let c = 1
  let seed = (a * seed + c) % m
  in (f64.u64 seed / f64.u64 m, seed)

let mat_dist : [12]f64 =
  [0.140, 0.052, 0.275, 0.134, 0.154, 0.064, 0.066, 0.055, 0.008, 0.015, 0.025, 0.013]
let mat_dist_probs =
  [0] ++ scan (+) 0 (drop 1 mat_dist)

let pick_mat (seed: seed) : (i32, seed) =
  let (roll, seed) = LCG_random_double seed
  let (i,_) =
    loop (i, continue) = (0,true) while i < 12 && continue do
    let running = mat_dist_probs[i]
    in if roll < running
       then (i, false)
       else (i+1, true)
  in (i32.i64 (i%12), seed)

let argmax [n] (xs: [n]f64): i64 =
  let max (x1, y1) (x2, y2) =
    if y1 < y2 then (x2, y2) else (x1, y1)
  in reduce max (-1, -f64.inf) (zip (iota n) xs) |> (.0)

let run_event_based_simulation [length_num_nucs]
                               [length_unionized_energy_array]
                               [max_num_nucs]
                               [n_isotopes]
                               [n_gridpoints]
                               (inp: inputs)
                               (sd: simulation_data [length_num_nucs]
                                                    [length_unionized_energy_array]
                                                    [max_num_nucs]
                                                    [n_isotopes]
                                                    [n_gridpoints]) =
  let f i =
    let seed = fast_forward_LCG STARTING_SEED (2*i)
    let (p_energy, seed) = LCG_random_double seed
    let (mat, _seed) = pick_mat seed
    in calculate_macro_xs p_energy mat
                          sd.num_nucs
                          sd.concs
                          sd.unionized_energy_array
                          sd.index_grid
                          sd.nuclide_grid
                          sd.mats
                          inp.grid_type
                          inp.hash_bins
  in tabulate inp.lookups f

let verification =
  let f (macro_xs_vector: (f64,f64,f64,f64,f64)) =
    let macro_xs_vector =
      [macro_xs_vector.0,
       macro_xs_vector.1,
       macro_xs_vector.2,
       macro_xs_vector.3,
       macro_xs_vector.4]
    in #[sequential] #[unroll] argmax macro_xs_vector + 1
  in map f >-> i64.sum >-> (%999983)

let unpack n_isotopes n_gridpoints grid_type hash_bins lookups
           num_nucs concs mats nuclide_grid index_grid unionized_energy_array
           : (inputs, simulation_data [][][][][]) =
  let grid_type = match grid_type : i64
                  case 0 -> #unionized
                  case 1 -> #nuclide
                  case _ -> #hash
  let inputs = {n_isotopes, n_gridpoints, grid_type, hash_bins, lookups}
  let unpack_nuclide (arr: [6]f64) =
    {energy = arr[0], total_xs = arr[1], elastic_xs = arr[2],
     absorbtion_xs = arr[3], fission_xs = arr[4], nu_fission_xs = arr[5]}
  let nuclide_grid = map (map unpack_nuclide) nuclide_grid
  let sd =
    {num_nucs, concs, mats, nuclide_grid, index_grid, unionized_energy_array}
  in (inputs, sd)

let main n_isotopes n_gridpoints grid_type hash_bins lookups
         num_nucs concs mats nuclide_grid index_grid unionized_energy_array =
  let (inputs, sd) =
    unpack n_isotopes n_gridpoints grid_type hash_bins lookups
           num_nucs concs mats nuclide_grid index_grid unionized_energy_array
  in #[unsafe] verification (run_event_based_simulation inputs sd)

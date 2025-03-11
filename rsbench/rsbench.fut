-- ==
-- tags { no_opencl }
-- entry: main
-- input @ data/small.in.gz output { 880018i64 }
-- input @ data/large.in.gz output { 358389i64 }

-- ==
-- entry: diff
-- input @ data/small.in.gz
-- input @ data/large.in.gz

type input =
  { lookups: i64,
    doppler: i32
  }

type window = {T: f64, A: f64, F: f64, start: i32, end: i32}

type rs_complex = {r: f64, i: f64}

def c_add (A: rs_complex) (B: rs_complex) : rs_complex =
  {r=A.r + B.r, i=A.i + B.i}

def c_sub (A: rs_complex) (B: rs_complex) : rs_complex =
  {r=A.r - B.r, i=A.i - B.i}

def c_mul (A: rs_complex) (B: rs_complex) : rs_complex =
  let a = A.r
  let b = A.i
  let c = B.r
  let d = B.i
  in {r=a*c - b*d, i=a*d + b*c}

def c_div ({r=a,i=b}: rs_complex) ({r=c,i=d}: rs_complex) : rs_complex =
  let denom = c*c + d*d
  in {r = (a*c + b*d) / denom,
      i = (b*c - a*d) / denom}

def c_abs (A: rs_complex) : f64 =
  f64.sqrt(A.r*A.r + A.i*A.i)

def fast_exp (x: f64) : f64 =
  let x = 1 + x * 0.000244140625
  let x = x * x let x = x * x  let x = x * x let x = x * x
  let x = x * x let x = x * x  let x = x * x let x = x * x
  let x = x * x let x = x * x  let x = x * x let x = x * x
                                             in x

def fast_cexp ({r=x,i=y}: rs_complex) : rs_complex =
  let t1 = fast_exp x
  let t2 = f64.cos y
  let t3 = f64.sin y
  let t4 = {r=t2, i=t3}
  let t5 = {r=t1, i=0}
  in t5 `c_mul` t4

type pole = {mp_ea: rs_complex,
             mp_rt: rs_complex,
             mp_ra: rs_complex,
             mp_rf: rs_complex,
             l_value: i16}

type~ simulation_data =
     ?[max_windows]
      [max_poles]
      [length_num_nucs]
      [n_nuclides]
      [numL]
      [num_mats]
      [max_num_nucs].
  { n_windows: [n_nuclides]i32,
    poles: [n_nuclides][max_poles]pole,
    windows: [n_nuclides][max_windows]window,
    pseudo_K0RS: [n_nuclides][numL]f64,
    num_nucs: [length_num_nucs]i32,
    mats: [num_mats][max_num_nucs]i32,
    concs: [length_num_nucs][max_num_nucs]f64
  }

type seed = u64

def STARTING_SEED : seed = 1070

def fast_forward_LCG (seed: seed) (n: i64) : seed =
  let m = 9223372036854775808 : u64
  let a = 2806196910506780709 : u64
  let c = 1 : u64
  let n = u64.i64 n % m
  let (_, _, _, a_new, c_new) =
    loop (n, a, c, a_new, c_new) = (n, a, c, 1, 0) for _i < 64-u64.clz n do
    let (a_new, c_new) =
      if n & 1 == 1
      then (a_new * a, c_new * a + c)
      else (a_new, c_new)
    in (n>>1, a * a, c * (a+1), a_new, c_new)
  in (a_new * seed + c_new) % m

def LCG_random_double (seed: seed) : (f64, seed) =
  let m = 9223372036854775808
  let a = 2806196910506780709
  let c = 1
  let seed = (a * seed + c) % m
  in (f64.u64 seed / f64.u64 m, seed)

def mat_dist : [12]f64 =
  [0.140, 0.052, 0.275, 0.134, 0.154, 0.064, 0.066, 0.055, 0.008, 0.015, 0.025, 0.013]
def mat_dist_probs =
  [0] ++ scan (+) 0 (drop 1 mat_dist)

def pick_mat (seed: seed) : (i32, seed) =
  let (roll, seed) = LCG_random_double seed
  let (i,_) =
    loop (j, continue) = (0,true) for i < 12 do
      if !continue then (j, continue)
      else if roll < mat_dist_probs[i]
      then (i, false)
      else (i+1, true)
  in (i32.i64 (i%12), seed)

def calculate_sig_T (nuc: i32) (E: f64) (data: simulation_data)
  : [4]rs_complex =
  let f i =
    let phi = data.pseudo_K0RS[nuc,i] * f64.sqrt E
    let phi = match i case 1 -> phi - -(f64.atan phi)
                      case 2 -> phi - f64.atan (3*phi/(3-phi*phi))
                      case 3 -> phi - f64.atan(phi*(15-phi*phi)/(15-6*phi*phi))
                      case _ -> phi
    let phi = 2 * phi
    in {r=f64.cos phi, i = -f64.sin(phi)}
  in [f 0, f 1, f 2, f 3]

def fast_nuclear_W (Z: rs_complex) : rs_complex =
  if c_abs Z < 6
  then let prefactor = {r=0, i=8.124330e+01}
       let an = [ 2.758402e-01,
		  2.245740e-01,
		  1.594149e-01,
		  9.866577e-02,
		  5.324414e-02,
		  2.505215e-02,
		  1.027747e-02,
		  3.676164e-03,
		  1.146494e-03,
		  3.117570e-04
                ]
       let neg_1n =
         [ -1.0,
	   1.0,
	   -1.0,
	   1.0,
	   -1.0,
	   1.0,
	   -1.0,
	   1.0,
	   -1.0,
	   1.0
         ]
       let denominator_left =
         [ 9.869604e+00,
	   3.947842e+01,
	   8.882644e+01,
	   1.579137e+02,
	   2.467401e+02,
	   3.553058e+02,
	   4.836106e+02,
	   6.316547e+02,
	   7.994380e+02,
	   9.869604e+02
         ]
       let t1 = {r=0,i=12}
       let t2 = {r=12,i=0}
       let i = {r=0,i=1}
       let one = {r=1,i=0}
       let W = (i `c_mul` (one `c_sub` fast_cexp(t1 `c_mul` Z))) `c_div` (t2 `c_mul` Z)
       let sum = loop sum = {r=0,i=0} for n < 10 do
                 let t3 = {r=neg_1n[n], i=0}
                 let top = (t3 `c_mul` fast_cexp(t1 `c_mul` Z)) `c_sub` one
                 let t4 = {r=denominator_left[n], i=0}
                 let t5 = {r=144, i=0}
                 let bot = t4 `c_sub` (t5 `c_mul` (Z `c_mul` Z))
                 let t6 = {r=an[n], i=0}
                 in sum `c_add` (t6 `c_mul` (top `c_div` bot))
       in W `c_add` (prefactor `c_mul` (Z `c_mul` sum))
  else
  let a = {r=0.512424224754768462984202823134979415014943561548661637413182,i=0}
  let b = {r=0.275255128608410950901357962647054304017026259671664935783653,i=0}
  let c = {r=0.051765358792987823963876628425793170829107067780337219430904,i=0}
  let d = {r=2.724744871391589049098642037352945695982973740328335064216346,i=0}
  let i = {r=0,i=1}
  let Z2 = Z `c_mul` Z
  let W = (Z `c_mul` i)
          `c_mul`
          ((a `c_div` (Z2 `c_sub` b)) `c_add` (c `c_div` (Z2 `c_sub` d)))
  in W

def calculate_micro_xs_doppler (nuc: i32) (E: f64) (data: simulation_data)
  : (f64, f64, f64, f64) =
  let spacing = 1 / f64.i32 data.n_windows[nuc]
  let window = i32.min (data.n_windows[nuc]-1) (i32.f64 (E / spacing))
  let sigTfactors = calculate_sig_T nuc E data
  let w = data.windows[nuc, window]
  let dopp = 0.5
  let (sigT, sigA, sigF) =
    loop (sigT, sigA, sigF) = (E * w.T,E * w.A,E * w.F)
    for i in w.start..<w.end do
    let pole = data.poles[nuc,i]
    let E_c = {r=E, i=0}
    let dopp_c = {r=dopp, i=0}
    let Z = (E_c `c_sub` pole.mp_ea) `c_mul` dopp_c
    let faddeeva = fast_nuclear_W Z
    in (sigT + (pole.mp_rt `c_mul` (faddeeva `c_mul` sigTfactors[pole.l_value])).r,
        sigA + (pole.mp_ra `c_mul` faddeeva).r,
        sigF + (pole.mp_rf `c_mul` faddeeva).r)
  let sigE = sigT - sigA
  in (sigT, sigA, sigF, sigE)

def calculate_micro_xs (nuc: i32) (E: f64) (data: simulation_data)
  : (f64, f64, f64, f64) =
  let spacing = 1 / f64.i32 data.n_windows[nuc]
  let window = i32.min (data.n_windows[nuc]-1) (i32.f64 (E / spacing))
  let sigTfactors = calculate_sig_T nuc E data
  let w = data.windows[nuc, window]
  let (sigT, sigA, sigF) =
    loop (sigT, sigA, sigF) = (E * w.T,E * w.A,E * w.F)
    for i in w.start..<w.end do
    let pole = data.poles[nuc,i]
    let t1 = {r=0,i=1}
    let t2 = {r=f64.sqrt(E), i=0}
    let PSIIKI = t1 `c_div` (pole.mp_ea `c_sub` t2)
    let E_c = {r=E, i=0}
    let CDUM = PSIIKI `c_div` E_c
    in (sigT + (pole.mp_rt `c_mul` (CDUM `c_mul` sigTfactors[pole.l_value])).r,
        sigA + (pole.mp_ra `c_mul` CDUM).r,
        sigF + (pole.mp_rf `c_mul` CDUM).r)
  let sigE = sigT - sigA
  in (sigT, sigA, sigF, sigE)

def calculate_macro_xs (mat: i32) (E: f64) (doppler: i32)
                       (data: simulation_data) : (f64, f64, f64, f64) =
  loop macro_xs = (0,0,0,0) for i < data.num_nucs[mat] do
  let nuc = data.mats[mat,i]
  let micro_xs = if doppler == 1
                 then calculate_micro_xs_doppler nuc E data
                 else calculate_micro_xs nuc E data
  in (macro_xs.0 + micro_xs.0 * data.concs[mat,i],
      macro_xs.1 + micro_xs.1 * data.concs[mat,i],
      macro_xs.2 + micro_xs.2 * data.concs[mat,i],
      macro_xs.3 + micro_xs.3 * data.concs[mat,i])

def argmax [n] (xs: [n]f64): i64 =
  let max (x1, y1) (x2, y2) =
    if y1 < y2 then (x2, y2) else (x1, y1)
  in reduce max (-1, -f64.inf) (zip (iota n) xs) |> (.0)

def run_event_based_simulation lookups doppler (sd: simulation_data) =
  let f i =
    let seed = fast_forward_LCG STARTING_SEED (2*i)
    let (E, seed) = LCG_random_double seed
    let (mat, _seed) = pick_mat seed
    in calculate_macro_xs mat E doppler sd
  in tabulate lookups f

def verification =
  let f (macro_xs_vector: (f64,f64,f64,f64)) =
    let macro_xs_vector =
      [macro_xs_vector.0,
       macro_xs_vector.1,
       macro_xs_vector.2,
       macro_xs_vector.3]
    in #[sequential] #[unroll] argmax macro_xs_vector + 1
  in map f >-> i64.sum >-> (%999983)

def unpack lookups doppler
           n_windows poles_ls poles_cs windows_f64s windows_i32s pseudo_K0RS num_nucs mats concs:
           (input, simulation_data)=
  let complex a = {r=a[0], i=a[1]}
  let unpack_pole l_value (cs: [4][2]f64) : pole =
    {l_value,
     mp_ea = complex cs[0],
     mp_rt = complex cs[1],
     mp_ra = complex cs[2],
     mp_rf = complex cs[3]}
  let poles = map2 (map2 unpack_pole) poles_ls poles_cs
  let unpack_window (f64s: [3]f64) (i32s: [2]i32) : window =
    {T=f64s[0], A=f64s[1], F=f64s[2], start=i32s[0], end=i32s[1]}
  let windows = map2 (map2 unpack_window) windows_f64s windows_i32s
  let input = {lookups, doppler}
  let sd = {n_windows, poles, windows, pseudo_K0RS, num_nucs, mats, concs}
  in (input, sd)

def main lookups doppler
         n_windows poles_ls poles_cs windows_f64s windows_i32s pseudo_K0RS num_nucs mats concs =
  let (input, sd) = unpack lookups doppler
                           n_windows poles_ls poles_cs windows_f64s windows_i32s pseudo_K0RS num_nucs mats concs
  in #[unsafe] verification (run_event_based_simulation input.lookups input.doppler sd)

entry diff lookups doppler
           n_windows poles_ls poles_cs windows_f64s windows_i32s pseudo_K0RS num_nucs mats concs =
  let (input, sd) = unpack lookups doppler
                           n_windows poles_ls poles_cs windows_f64s windows_i32s pseudo_K0RS num_nucs mats concs
  let diff_res = (vjp (run_event_based_simulation input.lookups input.doppler)
                      sd
                      (replicate input.lookups (1,1,1,1))).poles
  in (map (map (.l_value)) diff_res,
      map (map (.mp_ea.i)) diff_res,
      map (map (.mp_ea.r)) diff_res,
      map (map (.mp_ra.i)) diff_res,
      map (map (.mp_ra.r)) diff_res,
      map (map (.mp_rf.i)) diff_res,
      map (map (.mp_rt.r)) diff_res,
     )

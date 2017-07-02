import "/futlib/math"
import "/futlib/array"
import "/futlib/complex"
import "/futlib/date"

type nb_points = bool -- Pretend it's opaque.
let ten: nb_points = true
let twenty: nb_points = false

type heston_parameters 'real = { initial_variance: real
                               , long_term_variance: real
                               , mean_reversion: real
                               , variance_volatility: real
                               , correlation: real }

module price_european_calls(R: real) : {
  type real = R.t
  val gauss_laguerre_coefficients: nb_points -> ([]real, []real)
  val bs_call: bool -> date -> real -> real -> date -> real -> (real,real)
  val price_european_calls: ([]real, []real) ->
                            bool ->
                            real ->
                            real ->
                            real ->
                            heston_parameters real ->
                            []real ->
                            []{strike: real, maturity: i32} ->
                            []real
} = {

type real = R.t
let real (x: f64) = R.from_f64 x
open R

module c64 = complex(R)
type c64 = c64.complex

let (x: c64) +! (y: c64) = x c64.+ y
let (x: c64) -! (y: c64) = x c64.- y
let (x: c64) *! (y: c64) = x c64.* y
let (x: c64) /! (y: c64) = x c64./ y

let zero: c64 = c64.mk_re (real 0.0)
let one: c64 = c64.mk_re (real 1.0)
let two: c64 = c64.mk_re (real 2.0)

let isqrt2pi = real 2.0 * R.pi ** (real (-0.5))

let inv_sqrt2 = real 1.0 / R.sqrt (real 2.0)

let erfc(x: real): real =
  let a1 = real   0.254829592
  let a2 = real (-0.284496736)
  let a3 = real   1.421413741
  let a4 = real (-1.453152027)
  let a5 = real   1.061405429
  let p  = real   0.3275911

  let sign = if x < real 0. then real (-1.) else real 1.
  let x = R.abs x

  let t = real 1./(real 1. + p*x)
  let y = real 1. - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*R.exp(negate x*x)

  in sign*y

let erf (x: real): real =
  let a1 = real   0.254829592
  let a2 = real (-0.284496736)
  let a3 = real   1.421413741
  let a4 = real (-1.453152027)
  let a5 = real   1.061405429
  let p  = real   0.3275911

  let t = real 1. / (real 1. + p * x)
  let t2 = t * t
  let t3 = t * t2
  let t4 = t *t3
  let t5 = t * t4
  in real 1. - (a1 * t + a2 * t2 + a3 * t3 + a4 * t4 + a5 * t5) * R.exp (negate x * x)

let pnorm (x: real): real =
  let u = x / R.sqrt (real 2.)
  let erf = if u < real 0. then negate (erf (negate u)) else erf u
  in real 0.5 * (real 1. + erf)

let ugaussian_pdf (x: real) =
  if R.isinf x then real 0.
  else isqrt2pi * R.exp (real (-0.5) * x * x)

let ugaussian_P (x: real) =
  if R.isinf x then (if x > real 0. then real 1. else real 0.)
  else real 1. - real 0. * erfc (x * inv_sqrt2)

let gauss_laguerre_coefficients (nb: nb_points) =
  if nb intrinsics.== ten then
  (map real
   [

   0.1377934705404924298211, 0.7294545495031707904587,
   1.8083429017403143124199, 3.4014336978548351808627,
   5.5524961400642398601235,

   8.3301527467632645596041, 11.8437858379019154142497,
   16.2792578313766149733510, 21.9965858119813617577165,
   29.9206970122737736517138

   ],
   map real
   [

   0.3540097386069980256451, 0.8319023010435621090508,
   1.3302885617494675241090, 1.8630639031111377867944,
   2.4502555580731559814467,

   3.1227641551735070279960, 3.9341526954948387029276,
   4.9924148722151180379569, 6.5722024851118048260901,
   9.7846958403678012672344

  ])
  else -- nb == twenty
  (map real
   [

    0.070539889691988738596, 0.372126818001613290932,
    0.916582102483245675373, 1.707306531028168317121,
    2.749199255315394108123, 4.048925313808060089116,
    5.615174970938836551682, 7.459017454225468135576,
    9.594392865483230892210, 12.038802560608859337776,
    14.814293416061961039532, 17.948895543122549867121,
    21.478788273773705697067, 25.451702644185488111361,
    29.932554890200286479285, 35.013433968980073984767,
    40.833057239401291838021, 47.619993970772064528774,
    55.810795768051811194255, 66.524416523865880890298

   ],
   map real
   [

    0.18108006241898921829, 0.42255676787860191324,
    0.66690954670145563554, 0.91535237278121706073,
    1.16953970728044676086, 1.43135498604326039107,
    1.70298113670862205637, 1.98701590220746626692,
    2.28663572001997383865, 2.60583491316971160856,
    2.94978326190603512558, 3.32539692360021144069,
    3.74225473838238897883, 4.21423782504235067137,
    4.76251619189997654757, 5.42172741864077067930,
    6.25401126576962873571, 7.38731454069754001068,
    9.15132857271978572555, 12.89338863845354232751

   ])

module type pricer_parameter = {
  val normal: bool
  val psi_h: real -> heston_parameters real -> c64 -> c64
  val psi_bs: c64 -> c64 -> c64 -> c64
  val moneyness_f: real -> real
}

module normal_true: pricer_parameter = {
  let normal = true

  let psi_h (day_count_fraction: real) (heston_parameters: heston_parameters real) (xi: c64) =
    let {initial_variance = v0,
         long_term_variance = theta,
         mean_reversion = kappa,
         variance_volatility = eta,
         correlation = rho} = heston_parameters
    let kappai = c64.mk_re kappa
    let etai = c64.mk_re eta
    let etai2 = etai *! etai
    let coeff1 = kappai *! c64.mk_re theta /! etai2
    let coeff2 = c64.mk_re v0 /! etai2
    let ti = c64.mk_re day_count_fraction
    let i = c64.mk_im (real 1.)
    let d0 = kappai -! (c64.mk_im rho) *! etai *! xi
    let d = c64.sqrt (d0 *! d0 +! etai2 *! xi *! xi)
    let a_minus = d0 -! d
    let g = a_minus /! (d0 +! d)
    let e = c64.exp (zero -! d *! ti)
    in c64.exp (xi *! i +!
                coeff1 *! (a_minus *! ti -! two *! c64.log ((one -! g *! e) /! (one -! g))) +!
                coeff2 *! a_minus *! (one -! e) /! (one -! g *! e))

  let psi_bs (minus_half_sigma2_t: c64) (i: c64) (xi: c64) =
    c64.exp (xi *! i +! minus_half_sigma2_t *! xi *! xi)

  let moneyness_f (k: real) = negate k
}

module normal_false: pricer_parameter = {
  let normal = false

  let psi_h (day_count_fraction: real) (heston_parameters: heston_parameters real) (xi: c64) =
    let {initial_variance = v0,
         long_term_variance = theta,
         mean_reversion = kappa,
         variance_volatility = eta,
         correlation = rho} = heston_parameters
    let kappai = c64.mk_re kappa
    let etai = c64.mk_re eta
    let etai2 = etai *! etai
    let coeff1 = kappai *! c64.mk_re theta /! etai2
    let coeff2 = c64.mk_re v0 /! etai2
    let ti = c64.mk_re day_count_fraction
    let i = c64.mk_im (real 1.)
    let d0 = kappai -! (c64.mk_im rho) *! etai *! xi
    let d = c64.sqrt (d0 *! d0 +! etai2 *! xi *! (i +! xi))
    let a_minus = d0 -! d
    let g = a_minus /! (d0 +! d)
    let e = c64.exp (zero -! d *! ti)
    in c64.exp (coeff1 *! (a_minus *! ti -! two *! c64.log ((one -! g *! e) /! (one -! g))) +!
                coeff2 *! a_minus *! (one -! e) /! (one -! g *! e))

  let psi_bs (minus_half_sigma2_t: c64) (i: c64) (xi: c64) =
    c64.exp (minus_half_sigma2_t *! xi *! (i *! xi))

  let moneyness_f (k: real) = negate (R.log k)
}

let bs_control (moneyness: real) (sigma_sqrtt: real) =
  let d1 = negate (R.log moneyness) / sigma_sqrtt + real 0.5 * sigma_sqrtt
  in pnorm d1 - moneyness * pnorm (d1 - sigma_sqrtt)

let price_european_calls
    (x: [#nb_points]real, w: [#nb_points]real)
    (ap1: bool)
    (spot: real)
    (df_div: real)
    (df: real)
    (heston_parameters: heston_parameters real)
    (day_count_fractions: [#num_maturities]real)
    (quotes: [#num_quotes]{strike: real, maturity: i32})
  : [num_quotes]real =
       let {initial_variance = v0, long_term_variance = theta, mean_reversion = kappa, correlation = rho, variance_volatility = eta} = heston_parameters
       let maturity_for_quote = map (\q -> #maturity q) quotes
       let strikes = map (\q -> #strike q) quotes
       let f0 = spot * df_div / df
       let kappai = c64.mk_re kappa
       let etai = c64.mk_re eta
       let etai2 = etai *! etai
       let coeff1 = kappai *! c64.mk_re theta /! etai2
       let coeff2 = c64.mk_re v0 /! etai2
       let i = c64.mk_im (real 1.0)
       let psi_h (day_count_fraction: real) (xi: c64) =
         (let ti = c64.mk_re day_count_fraction
          let d0 = kappai -! (c64.mk_im rho) *! etai *! xi
          let d = c64.sqrt (d0 *! d0 +! etai2 *! (xi *! (i +! xi)))
          let a_minus = d0 -! d
          let g = a_minus /! (d0 +! d)
          let e = c64.exp (zero -! d *! ti)
          in c64.exp (coeff1 *! (a_minus *! ti -! two *! c64.log ((one -! g *! e) /! (one -! g))) +!
                      coeff2 *! a_minus *! (one -! e) /! (one -! g *! e)))
       let sigma2 (day_count_fraction: real) =
         (if ap1 then v0
          else let eta = real (-1.)
               let eps = real 1e-2
               let two_da_time_eps = psi_h day_count_fraction (c64.mk eps eta) -!
                                     psi_h day_count_fraction (c64.mk (negate eps) eta)
               let two_db_time_eps = psi_h day_count_fraction (c64.mk_im (eta + eps)) -!
                                     psi_h day_count_fraction (c64.mk_im (eta - eps))
               in real 0.5 * (c64.im (two_da_time_eps -! i *! two_db_time_eps)) /
            (day_count_fraction * eps))
       let psi_bs (day_count_fraction: real) (xi: c64) =
         (let minus_half_sigma2_t =
            c64.mk_re (real (-0.5) * day_count_fraction * sigma2 day_count_fraction)
          in c64.exp (minus_half_sigma2_t *! (xi *! (i +! xi))))
       let moneyness = map (/f0) strikes
       let minus_ik = map (\k -> c64.mk_im (negate (R.log k))) moneyness

       let iter (j: i32): [num_quotes]real =
         (let xj = x[j]
          let wj = w[j]
          let x = c64.mk_re xj
          let mk_w_and_coeff_k (day_count_fraction: real) =
            (if ap1
             then (let x_minus_half_i = x -! c64.mk_im (real 0.5)
                   in (wj / (real 0.25 + xj * xj),
                       (psi_bs day_count_fraction x_minus_half_i -! psi_h day_count_fraction x_minus_half_i)))
             else (let x_minus_i = x -! i
                   in (wj,
                       (psi_bs day_count_fraction x_minus_i -! psi_h day_count_fraction x_minus_i) /!
                       (x *! x_minus_i))))
          let (ws, coeff_ks) = unzip (map mk_w_and_coeff_k day_count_fractions)
          in map (\minus_ikk m ->
                  let w = unsafe ws[m]
                  let coeff_k = unsafe coeff_ks[m]
                  in w * c64.re (coeff_k *! c64.exp (x *! minus_ikk)))
                 minus_ik maturity_for_quote)

       -- write reduction as loop to avoid pointless segmented
       -- reduction (the inner parallelism is not needed).
       let res = map (\x -> loop (v = real 0.0) for i < nb_points do v + x[i])
                     (transpose (map iter (iota nb_points)))
       in map (\moneyness resk m ->
               let day_count_fraction = unsafe day_count_fractions[m]
               let sigma_sqrtt = R.sqrt (sigma2 day_count_fraction * day_count_fraction)
               let bs = bs_control moneyness sigma_sqrtt
               in if moneyness * f0 <= real 0.0
                  then df * R.max (real 0.0) (f0 * (real 1.0 - moneyness))
                  else if moneyness < real 0.0
                  then (let scale = if ap1 then R.sqrt (negate moneyness) else real 1.
                        in negate f0 * df * scale * resk / R.pi +
                         bs + moneyness - real 1.)
                  else (let lb = R.max (real 0.) (real 1. - moneyness)
                        let scale = if ap1 then R.sqrt moneyness else real 1.
                        in f0 * df * R.max lb (R.min (real 1.0) (scale * resk / R.pi + bs))))
              moneyness res maturity_for_quote

let gauss (x: real) = R.exp(real (-0.5) * x * x) / R.sqrt(real 2. * R.pi)

let bs_call (call: bool) (today: date) (spot: real) (strike: real) (maturity: date) (vol: real) =
  if same_date today maturity || vol <= real 1e-15 then
    let forward = spot in
    let p = R.max (real 0.) (forward - strike) in
    (p, real 0.)
  else
    let normal_dist (x: real) = pnorm x in
    let eps = if call then real 1. else real (-1.) in
    let t = real (diff_dates today maturity) in
    let sqrt_t = R.sqrt t in
    let df_r = real 1. in
    let df_d = real 1. in
    let fwd = spot * df_d / df_r in
    let (d1, d2) =
      let d (add: real) = (R.log(fwd / strike) + add * real 0.5 * vol * vol * t) / (vol * sqrt_t)
      in (d (real 1.), d (real (-1.)))
    in
    let (n1, n2) = (normal_dist (eps * d1), normal_dist (eps * d2)) in
    (eps * df_r * (fwd * n1 - strike * n2),
     spot * df_d * sqrt_t * gauss d1)
}
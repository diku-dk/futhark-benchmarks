-- Black-Scholes analytical option pricing engine.
-- Ported from FinanceBench (cavazos-lab/FinanceBench).
--
-- Each option is priced independently using the Black-Scholes-Merton
-- closed-form solution with cumulative normal distribution approximated
-- via the error function.
--
-- ==
-- entry: main
-- compiled input { 5000000i64 }
-- output { 30030504f32 }

-- Error function approximation (from QuantLib / Sun libm).
def erf (x: f32) : f32 =
  let tiny = 1e-21f32
  let one = 1.0f32
  let erx = 8.45062911510467529297e-01f32
  let efx = 1.28379167095512586316e-01f32
  let pp0 = 1.28379167095512558561e-01f32
  let pp1 = -3.25042107247001499370e-01f32
  let pp2 = -2.84817495755985104766e-02f32
  let pp3 = -5.77027029648944159157e-03f32
  let pp4 = -2.37630166566501626084e-05f32
  let qq1 = 3.97917223959155352819e-01f32
  let qq2 = 6.50222499887672944485e-02f32
  let qq3 = 5.08130628187576562776e-03f32
  let qq4 = 1.32494738004321644526e-04f32
  let qq5 = -3.96022827877536812320e-06f32
  let pa0 = -2.36211856075265944077e-03f32
  let pa1 = 4.14856118683748331666e-01f32
  let pa2 = -3.72207876035701323847e-01f32
  let pa3 = 3.18346619901161753674e-01f32
  let pa4 = -1.10894694282396677476e-01f32
  let pa5 = 3.54783043256182359371e-02f32
  let pa6 = -2.16637559486879084300e-03f32
  let qa1 = 1.06420880400844228286e-01f32
  let qa2 = 5.40397917702171048937e-01f32
  let qa3 = 7.18286544141962662868e-02f32
  let qa4 = 1.26171219808761642112e-01f32
  let qa5 = 1.36370839120290507362e-02f32
  let qa6 = 1.19844998467991074170e-02f32
  let ra0 = -9.86494403484714822705e-03f32
  let ra1 = -6.93858572707181764372e-01f32
  let ra2 = -1.05586262253232909814e+01f32
  let ra3 = -6.23753324503260060396e+01f32
  let ra4 = -1.62396669462573470355e+02f32
  let ra5 = -1.84605092906711035994e+02f32
  let ra6 = -8.12874355063065934246e+01f32
  let ra7 = -9.81432934416914548592e+00f32
  let sa1 = 1.96512716674392571292e+01f32
  let sa2 = 1.37657754143519042600e+02f32
  let sa3 = 4.34565877475229228821e+02f32
  let sa4 = 6.45387271733267880336e+02f32
  let sa5 = 4.29008140027567833386e+02f32
  let sa6 = 1.08635005541779435134e+02f32
  let sa7 = 6.57024977031928170135e+00f32
  let sa8 = -6.04244152148580987438e-02f32
  let rb0 = -9.86494292470009928597e-03f32
  let rb1 = -7.99283237680523006574e-01f32
  let rb2 = -1.77579549177547519889e+01f32
  let rb3 = -1.60636384855821916062e+02f32
  let rb4 = -6.37566443368389627722e+02f32
  let rb5 = -1.02509513161107724954e+03f32
  let rb6 = -4.83519191608651397019e+02f32
  let sb1 = 3.03380607434824582924e+01f32
  let sb2 = 3.25792512996573918826e+02f32
  let sb3 = 1.53672958608443695994e+03f32
  let sb4 = 3.19985821950859553908e+03f32
  let sb5 = 2.55305040643316442583e+03f32
  let sb6 = 4.74528541206955367215e+02f32
  let sb7 = -2.24409524465858183362e+01f32
  let ax = f32.abs x
  in if ax < 0.84375f32 then
       if ax < 3.7252902984e-09f32 then
         x + efx * x
       else
         let z = x * x
         let r = pp0 + z*(pp1 + z*(pp2 + z*(pp3 + z*pp4)))
         let s = one + z*(qq1 + z*(qq2 + z*(qq3 + z*(qq4 + z*qq5))))
         let y = r / s
         in x + x * y
     else if ax < 1.25f32 then
       let s = ax - one
       let p = pa0+s*(pa1+s*(pa2+s*(pa3+s*(pa4+s*(pa5+s*pa6)))))
       let q = one+s*(qa1+s*(qa2+s*(qa3+s*(qa4+s*(qa5+s*qa6)))))
       in if x >= 0.0f32 then erx + p/q else -erx - p/q
     else if ax >= 6.0f32 then
       if x >= 0.0f32 then one - tiny else tiny - one
     else
       let s = one / (ax * ax)
       let (r_val, s_val) =
         if ax < 2.85714285714285f32 then
           (ra0+s*(ra1+s*(ra2+s*(ra3+s*(ra4+s*(ra5+s*(ra6+s*ra7)))))),
            one+s*(sa1+s*(sa2+s*(sa3+s*(sa4+s*(sa5+s*(sa6+s*(sa7+s*sa8))))))))
         else
           (rb0+s*(rb1+s*(rb2+s*(rb3+s*(rb4+s*(rb5+s*rb6))))),
            one+s*(sb1+s*(sb2+s*(sb3+s*(sb4+s*(sb5+s*(sb6+s*sb7)))))))
       let r_result = f32.exp(-ax*ax - 0.5625f32 + r_val/s_val)
       in if x >= 0.0f32 then one - r_result/ax else r_result/ax - one

-- Cumulative normal distribution.
def cnd (x: f32) : f32 =
  0.5f32 * (1.0f32 + erf (x * 0.7071067811865475244f32))

-- Option types.
type option_type = #call | #put

-- Price a single option using Black-Scholes-Merton.
def black_scholes (otype: option_type) (strike: f32) (spot: f32)
                  (q: f32) (r: f32) (t: f32) (vol: f32) : f32 =
  let variance = vol * vol * t
  let std_dev = f32.sqrt variance
  let dividend_discount = f32.exp (-q * t)
  let risk_free_discount = f32.exp (-r * t)
  let forward = spot * dividend_discount / risk_free_discount
  let d1 = f32.log (forward / strike) / std_dev + 0.5f32 * std_dev
  let d2 = d1 - std_dev
  let (alpha, beta) =
    match otype
    case #call -> (cnd d1, -(cnd d2))
    case #put -> (cnd d1 - 1.0f32, 1.0f32 - cnd d2)
  in risk_free_discount * (forward * alpha + strike * beta)

-- The 37 different option configurations from FinanceBench.
def option_data : [37](option_type, f32, f32, f32, f32, f32, f32) =
  [ (#call,  40.00,  42.00, 0.08, 0.04, 0.75, 0.35)
  , (#call, 100.00,  90.00, 0.10, 0.10, 0.10, 0.15)
  , (#call, 100.00, 100.00, 0.10, 0.10, 0.10, 0.15)
  , (#call, 100.00, 110.00, 0.10, 0.10, 0.10, 0.15)
  , (#call, 100.00,  90.00, 0.10, 0.10, 0.10, 0.25)
  , (#call, 100.00, 100.00, 0.10, 0.10, 0.10, 0.25)
  , (#call, 100.00, 110.00, 0.10, 0.10, 0.10, 0.25)
  , (#call, 100.00,  90.00, 0.10, 0.10, 0.10, 0.35)
  , (#call, 100.00, 100.00, 0.10, 0.10, 0.10, 0.35)
  , (#call, 100.00, 110.00, 0.10, 0.10, 0.10, 0.35)
  , (#call, 100.00,  90.00, 0.10, 0.10, 0.50, 0.15)
  , (#call, 100.00, 100.00, 0.10, 0.10, 0.50, 0.15)
  , (#call, 100.00, 110.00, 0.10, 0.10, 0.50, 0.15)
  , (#call, 100.00,  90.00, 0.10, 0.10, 0.50, 0.25)
  , (#call, 100.00, 100.00, 0.10, 0.10, 0.50, 0.25)
  , (#call, 100.00, 110.00, 0.10, 0.10, 0.50, 0.25)
  , (#call, 100.00,  90.00, 0.10, 0.10, 0.50, 0.35)
  , (#call, 100.00, 100.00, 0.10, 0.10, 0.50, 0.35)
  , (#call, 100.00, 110.00, 0.10, 0.10, 0.50, 0.35)
  , (#put,  100.00,  90.00, 0.10, 0.10, 0.10, 0.15)
  , (#put,  100.00, 100.00, 0.10, 0.10, 0.10, 0.15)
  , (#put,  100.00, 110.00, 0.10, 0.10, 0.10, 0.15)
  , (#put,  100.00,  90.00, 0.10, 0.10, 0.10, 0.25)
  , (#put,  100.00, 100.00, 0.10, 0.10, 0.10, 0.25)
  , (#put,  100.00, 110.00, 0.10, 0.10, 0.10, 0.25)
  , (#put,  100.00,  90.00, 0.10, 0.10, 0.10, 0.35)
  , (#put,  100.00, 100.00, 0.10, 0.10, 0.10, 0.35)
  , (#put,  100.00, 110.00, 0.10, 0.10, 0.10, 0.35)
  , (#put,  100.00,  90.00, 0.10, 0.10, 0.50, 0.15)
  , (#put,  100.00, 100.00, 0.10, 0.10, 0.50, 0.15)
  , (#put,  100.00, 110.00, 0.10, 0.10, 0.50, 0.15)
  , (#put,  100.00,  90.00, 0.10, 0.10, 0.50, 0.25)
  , (#put,  100.00, 100.00, 0.10, 0.10, 0.50, 0.25)
  , (#put,  100.00, 110.00, 0.10, 0.10, 0.50, 0.25)
  , (#put,  100.00,  90.00, 0.10, 0.10, 0.50, 0.35)
  , (#put,  100.00, 100.00, 0.10, 0.10, 0.50, 0.35)
  , (#put,  100.00, 110.00, 0.10, 0.10, 0.50, 0.35)
  ]

-- Price n options by cycling through the 37 configurations.
-- Returns the sum of all option prices (as in the reference implementation).
entry main (n: i64) : f32 =
  let prices =
    map (\i ->
           let (otype, strike, spot, q, r, t, vol) = option_data[i % 37]
           in black_scholes otype strike spot q r t vol)
        (iota n)
  in f32.sum prices

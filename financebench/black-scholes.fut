-- Black-Scholes analytical option pricing engine.
-- Ported from FinanceBench (cavazos-lab/FinanceBench).
--
-- Each option is priced independently using the Black-Scholes-Merton
-- closed-form solution with cumulative normal distribution approximated
-- via the error function.
--
-- Note that the expected output here is when using parallel execution. Due to
-- roundoff error (and the use of single precision), the result is different
-- (worse!) when using sequential execution.
--
-- ==
-- entry: main
-- compiled input { 5000000i64 }
-- output { 30225740f32 }

-- Cumulative normal distribution.
def cnd (x: f32) : f32 =
  0.5f32 * (1.0f32 + f32.erf (x * 0.7071067811865475244f32))

-- Option types.
type option_type = #call | #put

-- Price a single option using Black-Scholes-Merton.
def black_scholes (otype: option_type)
                  (strike: f32)
                  (spot: f32)
                  (q: f32)
                  (r: f32)
                  (t: f32)
                  (vol: f32) : f32 =
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
  [ (#call, 40.00, 42.00, 0.08, 0.04, 0.75, 0.35)
  , (#call, 100.00, 90.00, 0.10, 0.10, 0.10, 0.15)
  , (#call, 100.00, 100.00, 0.10, 0.10, 0.10, 0.15)
  , (#call, 100.00, 110.00, 0.10, 0.10, 0.10, 0.15)
  , (#call, 100.00, 90.00, 0.10, 0.10, 0.10, 0.25)
  , (#call, 100.00, 100.00, 0.10, 0.10, 0.10, 0.25)
  , (#call, 100.00, 110.00, 0.10, 0.10, 0.10, 0.25)
  , (#call, 100.00, 90.00, 0.10, 0.10, 0.10, 0.35)
  , (#call, 100.00, 100.00, 0.10, 0.10, 0.10, 0.35)
  , (#call, 100.00, 110.00, 0.10, 0.10, 0.10, 0.35)
  , (#call, 100.00, 90.00, 0.10, 0.10, 0.50, 0.15)
  , (#call, 100.00, 100.00, 0.10, 0.10, 0.50, 0.15)
  , (#call, 100.00, 110.00, 0.10, 0.10, 0.50, 0.15)
  , (#call, 100.00, 90.00, 0.10, 0.10, 0.50, 0.25)
  , (#call, 100.00, 100.00, 0.10, 0.10, 0.50, 0.25)
  , (#call, 100.00, 110.00, 0.10, 0.10, 0.50, 0.25)
  , (#call, 100.00, 90.00, 0.10, 0.10, 0.50, 0.35)
  , (#call, 100.00, 100.00, 0.10, 0.10, 0.50, 0.35)
  , (#call, 100.00, 110.00, 0.10, 0.10, 0.50, 0.35)
  , (#put, 100.00, 90.00, 0.10, 0.10, 0.10, 0.15)
  , (#put, 100.00, 100.00, 0.10, 0.10, 0.10, 0.15)
  , (#put, 100.00, 110.00, 0.10, 0.10, 0.10, 0.15)
  , (#put, 100.00, 90.00, 0.10, 0.10, 0.10, 0.25)
  , (#put, 100.00, 100.00, 0.10, 0.10, 0.10, 0.25)
  , (#put, 100.00, 110.00, 0.10, 0.10, 0.10, 0.25)
  , (#put, 100.00, 90.00, 0.10, 0.10, 0.10, 0.35)
  , (#put, 100.00, 100.00, 0.10, 0.10, 0.10, 0.35)
  , (#put, 100.00, 110.00, 0.10, 0.10, 0.10, 0.35)
  , (#put, 100.00, 90.00, 0.10, 0.10, 0.50, 0.15)
  , (#put, 100.00, 100.00, 0.10, 0.10, 0.50, 0.15)
  , (#put, 100.00, 110.00, 0.10, 0.10, 0.50, 0.15)
  , (#put, 100.00, 90.00, 0.10, 0.10, 0.50, 0.25)
  , (#put, 100.00, 100.00, 0.10, 0.10, 0.50, 0.25)
  , (#put, 100.00, 110.00, 0.10, 0.10, 0.50, 0.25)
  , (#put, 100.00, 90.00, 0.10, 0.10, 0.50, 0.35)
  , (#put, 100.00, 100.00, 0.10, 0.10, 0.50, 0.35)
  , (#put, 100.00, 110.00, 0.10, 0.10, 0.50, 0.35)
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

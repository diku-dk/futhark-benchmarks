import "/futlib/math"
import "/futlib/date"
import "/futlib/random"
import "least_squares"
import "price_european_calls"

module heston (real: real)
              (rand: rng_engine): {
  type real = real.t
  val heston: i32 -> i32 -> i32 -> i32 ->
              []i32 ->
              []real ->
              []real ->
              (real,i32,real,real,real,real,real)
} = {

type real = real.t
let int (x: i32) = real.i32 x

let (x: real) +. (y: real) = x real.+ y
let (x: real) *. (y: real) = x real.* y
let (x: real) -. (y: real) = x real.- y
let (x: real) /. (y: real) = x real.- y
let (x: real) <=. (y: real) = x real.<= y

let heston_parameters_from_vector (x: [5]real) =
  { initial_variance = x[0]
  , long_term_variance = x[1]
  , correlation = x[2]
  , mean_reversion = x[3]
  , variance_volatility = x[4] }

module price_european_calls_real = price_european_calls real

module heston_least_squares = least_squares real rand {
  open (relative_distance real)

  type quote = {maturity: i32, strike: real.t, vega: real.t, weight: real.t}

  type objective_ctx = {day_count_fractions: []real.t,
                        quotes: []quote,
                        gauss_laguerre_coefficients: ([]real.t, []real.t)
                        }

  let objective ({day_count_fractions, quotes, gauss_laguerre_coefficients}: objective_ctx) (x: []real): []real =
    let heston_parameters = heston_parameters_from_vector x
    let prices = price_european_calls_real.price_european_calls
                 gauss_laguerre_coefficients
                 false (int 1) (int 1) (int 1)
                 heston_parameters
                 day_count_fractions
                 (map (\(q: quote) -> {maturity=q.maturity, strike=q.strike}) quotes)
    in map2 (\(q: quote) p -> q.weight *. p /. q.vega) quotes prices
}

type quote = {maturity: date, strike: real, quote: real}

type calibration_input = { today: date
                         , quotes: []{maturity: date, strike: real, quote: real}
                         , max_global: i32
                         , np: i32
                         , strike_weight_bandwidth: real
                         , maturity_weight_x0: real
                         , maturity_weight_gamma: real
                         , integral_iterations: num_points
                         , variables: []optimization_variable real }

let distinct_maturities [n] (dates: [n]date): ([]date, [n]i32) =
  let switched (x: date) (i: i32) =
    i == 0 || unsafe !(same_date x dates[i - 1])
  let switches = map2 switched dates (iota n)
  in ((unzip (filter (\(x,_) -> x) (zip switches dates))).2,
      map (\x -> x-1) (scan (+) 0 (map i32.bool switches)))

let run_calibration({today,
                     quotes,
                     max_global,
                     np,
                     strike_weight_bandwidth,
                     maturity_weight_x0,
                     maturity_weight_gamma,
                     integral_iterations,
                     variables}: calibration_input): calibration_result real =
  let price_and_vega_of_quote (strike: real) (maturity: date) (quote: real) =
    (let (price, vega) = price_european_calls_real.bs_call true today (int 1) strike maturity quote
     in (price, real.max (real.f64 1e-1) vega))
  let strike_weight (p: real) (x: real) = real.exp (p *. (real.log x +. int 1 -. x))
  let maturity_weight (x0: real) (gamma: real) (x: real) =
      (let k = int 1 /. (real.exp(gamma *. x0) -. int 1)
       in if x <=. x0 then k *. (real.exp(gamma *. x) -. int 1) else int 1)
  let weight (strike: real) (mat: date) =
    maturity_weight maturity_weight_x0 maturity_weight_gamma (real.f64 (diff_dates today mat)) *.
    strike_weight strike_weight_bandwidth strike


  let (maturity_dates, quotes_to_maturities) =
    distinct_maturities (map (\(q: quote) -> q.maturity) quotes)
  let weights = map (\{maturity, strike, quote=_} -> weight strike maturity) quotes
  let prices_and_vegas = map (\{maturity, strike, quote} ->
                              price_and_vega_of_quote strike maturity quote) quotes
  let quotes_for_optimization = map2 (\(p,v) w -> w *. p /. v) prices_and_vegas weights
  let quotes_for_ctx =
    map4 (\{maturity=_, strike, quote=_} w (_,v) i -> { maturity = i
                                                      , strike = strike
                                                      , weight = w
                                                      , vega = v})
        quotes weights prices_and_vegas quotes_to_maturities

  let ctx = { day_count_fractions =
                map real.f64 (map (diff_dates today) maturity_dates)
            , quotes =
                quotes_for_ctx
            , gauss_laguerre_coefficients =
                price_european_calls_real.gauss_laguerre_coefficients integral_iterations }

  in heston_least_squares.least_squares ctx max_global np variables quotes_for_optimization

let date_of_int(x: i32) =
  let d = x%100
  let m = (x/100)%100
  let y = x/10000
  in date_of_triple (y, m, d)

let default_variables: []optimization_variable real =
  [heston_least_squares.optimize_value
   {lower_bound =  real.f64 1e-6, initial_value = real.f64 4e-2, upper_bound = int 1},
   heston_least_squares.optimize_value
   {lower_bound =  real.f64 1e-6, initial_value = real.f64 4e-2, upper_bound = int 1},
   heston_least_squares.optimize_value
   {lower_bound =  int (-1), initial_value = real.f64 (-0.5), upper_bound = int 0},
   heston_least_squares.optimize_value
   {lower_bound =  real.f64 1e-4, initial_value = real.f64 1e-2, upper_bound = int 4},
   heston_least_squares.optimize_value
   {lower_bound =  real.f64 1e-4, initial_value = real.f64 0.4, upper_bound = int 2}
  ]

let heston [num_quotes]
           (max_global: i32)
           (num_points: i32)
           (np: i32)
           (today: i32)
           (quotes_maturity: [num_quotes]i32)
           (quotes_strike: [num_quotes]real)
           (quotes_quote: [num_quotes]real) =
  let result =
    run_calibration { today = date_of_int today
                    , quotes = map3 (\m k q -> {maturity = date_of_int m, strike = k, quote = q})
                      quotes_maturity quotes_strike quotes_quote
                    , max_global = max_global
                    , np = np
                    , strike_weight_bandwidth = int 0
                    , maturity_weight_x0 = int 0
                    , maturity_weight_gamma = int 1
                    , integral_iterations = if num_points == 10 then ten else twenty
                    , variables = default_variables
                      }
  let { initial_variance,
        long_term_variance,
        correlation,
        mean_reversion,
        variance_volatility} = heston_parameters_from_vector result.parameters
  in (result.root_mean_squared_error,
      result.num_feval,
      initial_variance,
      long_term_variance,
      mean_reversion,
      variance_volatility,
      correlation
      )
}

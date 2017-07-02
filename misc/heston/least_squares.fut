import "/futlib/math"
import "/futlib/random"

module type distance = {
  type real
  val distance: []real -> []real -> real
}

module absolute_distance(R: real): distance with real = R.t = {
  type real = R.t

  open R

  let distance (quotes: [#num_quotes]real) (prices: [#num_quotes]real): real =
    let norm (price: real) (quote: real) =
      (let rel = (price - quote) / quote
       in rel * rel)
    in reduce (+) (R.from_i32 0) (map norm quotes prices)
}

module relative_distance(R: real): distance with real = R.t = {
  type real = R.t

  open R

  let distance (quotes: [#num_quotes]real) (prices: [#num_quotes]real): real =
    let norm (price: real) (quote: real) =
      (let dif = price - quote
       in dif * dif)
    let a = map norm quotes prices
    in reduce (+) (R.from_i32 0) (intrinsics.opaque a)
}

module type pricer = {
  type pricer_ctx
  type real
  val pricer: pricer_ctx -> []real -> []real

  include distance with real = real
}

type range 'real = {lower_bound: real,
                    upper_bound: real,
                    initial_value: real}

-- Pretend this is a sum type with two constructors.
type optimization_variable 'real = ( bool -- fixed?
                                   , real -- value if fixed
                                   , range real -- range if not fixed
                                   )

type calibration_result 'real = { parameters: []real,
                                  root_mean_squared_error: real,
                                  quoted_prices: []real,
                                  calibrated_prices: []real,
                                  nb_feval: i32 }

module least_squares (real: real)
                     (rand: rng_engine)
                     (P: pricer with real = real.t) : {
  val fixed_value: P.real -> optimization_variable P.real
  val optimize_value: range P.real -> optimization_variable P.real

  val least_squares: P.pricer_ctx -> i32 -> i32
                   -> []optimization_variable P.real
                   -> []P.real
                   -> calibration_result P.real
} = {
  type real = real.t
  open real

  module random_i32 = uniform_int_distribution i32 rand
  module random_real = uniform_real_distribution real rand

  let nrand (d: random_real.distribution) (rng: rand.rng) (n: i32) =
    let rngs = rand.split_rng n rng
    let (rngs', xs) = unzip (map (\rng -> random_real.rand d rng) rngs)
    in (rand.join_rng rngs', xs)

  let fixed_value (v: real): optimization_variable real =
    (true, v, {lower_bound=real.from_i32 0,
               upper_bound=real.from_i32 0,
               initial_value=real.from_i32 0})

  let optimize_value (r: range real): optimization_variable real =
    (false, real.from_i32 0, r)


  -- Parameterisation of how the randomised search takes place.
  type mutation = {np: i32, -- Population size
                   cr: real  -- Crossover probability [0,1]
                   }

  type termination = {max_iterations: i32, max_global: i32, target: real}

  type status = i32 -- Pretend it's opaque!
  let max_iterations_reached: status = 0
  let max_global_reached: status = 1
  let target_reached: status = 2

  type result = {x0: []real, f: real, nb_feval: i32, status: status}

  let active_vars (vars_to_free_vars: [#num_vars]i32)
                  (variables: [#num_vars]optimization_variable real)
                  (xs: [#num_active]real) =
    map (\fv (fixed,x,_) -> if fixed then x else unsafe xs[fv])
        vars_to_free_vars variables

  let min_and_idx (a:real,a_i:i32) (b:real,b_i:i32) =
    if      a < b         then (a,a_i)
    else if b < a         then (b,b_i)
    else if a_i i32.< b_i then (a, a_i)
    else                       (b, b_i)

  let optimize (pricer_ctx: P.pricer_ctx)
               (quotes: [#num_quotes]real)
               (vars_to_free_vars: [#num_vars]i32)
               (variables: [#num_vars]optimization_variable real)
               ({np, cr}: mutation)
               (lower_bounds: [#num_free_vars]real)
               (upper_bounds: [#num_free_vars]real)
               ({max_iterations,max_global,target}: termination): result =
    -- The objective function.  This could be factored out into a
    -- function argument (as a parametric module).
    let objective (x: [num_free_vars]real): real =
      P.distance quotes (P.pricer pricer_ctx (active_vars vars_to_free_vars variables x))

    let bounds = (real.from_i32 0, real.from_i32 1)
    let rng = rand.rng_from_seed [0x123]
    let rngs = rand.split_rng np rng
    let (rngs, rss) = unzip (map (\rng -> nrand bounds rng num_free_vars) rngs)
    let rng = rngs[0]
    let x = (let init_j (lower_bound: real) (upper_bound: real) (r: real) =
               lower_bound + (upper_bound-lower_bound) * r
             let init_i (rs: [num_free_vars]real) = map init_j lower_bounds upper_bounds rs
             in map init_i rss)
    let fx = map objective x
    let (fx0, best_idx) =
      reduce_comm min_and_idx (real.inf, 0) (zip (intrinsics.opaque fx) (iota np))

    let mutation (difw: real) (best_idx: i32) (x: [np][num_free_vars]real)
                 (rng: rand.rng) (i :i32) (x_i: [num_free_vars]real) =
      (-- We have to draw 'to_draw' distinct elements from 'x', and it
       -- can't be 'i'.  We do this with brute-force looping.
       let (rng,a) = random_i32.rand (0,np) rng
       let (rng,b) = random_i32.rand (0,np) rng
       let (rng,c) = random_i32.rand (0,np) rng
       let (rng,a) = loop ((rng,a)) while a i32.== i do random_i32.rand (0,np) rng
       let (rng,b) = loop ((rng,b)) while b i32.== i || b i32.== a do random_i32.rand (0,np) rng
       let (rng,c) = loop ((rng,c)) while c i32.== i || c i32.== a || c i32.== b do random_i32.rand (0,np) rng
       let (rng,r) = random_real.rand bounds rng
       let x_r1 = unsafe if r <= real.from_fraction 1 2 then x[best_idx] else x[a]
       let x_r2 = unsafe x[b]
       let x_r3 = unsafe x[c]
       let (rng,j0) = random_i32.rand (0,num_free_vars) rng
       let (rng,rs) = nrand bounds rng num_free_vars
       let auxs = map (+) x_r1 (map (difw*) (map (-) x_r2 x_r3))
       let v_i = map (\j r lower_bound upper_bound aux x_i_j ->
                      if (j i32.== j0 || r <= cr) && lower_bound <= aux && aux <= upper_bound
                      then aux
                      else x_i_j)
                     (iota num_free_vars) rs lower_bounds upper_bounds auxs x_i

       in (rng, v_i))

    let recombination (fx0: real) (best_idx: i32) (fx: [np]real)
                      (x: [np][num_free_vars]real) (v: [np][num_free_vars]real) =
      (let f_v = map objective v
       let fx' = map real.min f_v fx
       let x' = map (\f fx_i x_i v_i -> if f < fx_i then v_i else x_i)
                    f_v fx x v
       let (fx0', best_idx') =
         reduce_comm min_and_idx
                    (fx0, best_idx)
                    (zip (intrinsics.opaque f_v) (iota np))
       in (fx0', best_idx', fx', x'))

    -- We are not counting the numer of invocations of the objective
    -- function quite as in LexiFi's code (they use a closure that
    -- increments a counter), but we should be close.
    let (_,ncalls,nb_it,(_,_,_,x)) =
      loop ((rng, ncalls, nb_it, (fx0, best_idx, fx, x)) =
            (rng, np, max_iterations, (fx0, best_idx, fx, x)))
      while nb_it i32.> 0 && max_global i32.> ncalls && fx0 > target do
      (let (rng,differential_weight) = random_real.rand (real.from_fraction 1 2, real.from_i32 1) rng
       let rngs = rand.split_rng np rng
       let (rngs, v) = unzip (map (mutation differential_weight best_idx x) rngs (iota np) x)
       let rng = rngs[0]
       let (fx0, best_idx, fx, x) = recombination fx0 best_idx fx x v
       in (rng, ncalls i32.+ np, nb_it i32.- 1,
           (fx0, best_idx, fx, x)))
    let x0 = x[best_idx]
    let status = if      fx0 <= target           then target_reached
                 else if max_global i32.< ncalls then max_global_reached
                 else if nb_it i32.== 0          then max_iterations_reached
                 else 1337 -- never reached
    in {x0=x0, f=fx0, nb_feval=ncalls, status=status}

  let least_squares
      (pricer_ctx: P.pricer_ctx)
      (max_global: i32)
      (np: i32)
      (variables: [#num_vars]optimization_variable real)
      (quotes: [#num_quotes]real)
      : calibration_result real =
    let (free_vars_to_vars, free_vars) =
      unzip (filter (\(_, (fixed, _, _)) -> !fixed) (zip (iota num_vars) variables))
    let num_free_vars = (shape free_vars)[0]
    let vars_to_free_vars = scatter (replicate num_vars (-1))
                                    free_vars_to_vars (iota num_free_vars)
    let (x, lower_bounds, upper_bounds) =
      unzip (map (\(_, _, {initial_value, lower_bound, upper_bound}) ->
                  (initial_value, lower_bound, upper_bound)) free_vars)

    let rms_of_error (err: real) = real.sqrt(err * (real.from_i32 10000 / real.from_i32 num_quotes))

    let (x, nb_feval) =
      if max_global i32.> 0
      then let res = (optimize pricer_ctx quotes vars_to_free_vars variables
                      {np = np, cr = real.from_fraction 9 10} lower_bounds upper_bounds
                      {max_iterations = 0x7FFFFFFF,
                       max_global = max_global,
                       target = real.from_i32 0})
           in (#x0 res, #nb_feval res)
      else (x, 0)

    let prices = P.pricer pricer_ctx (active_vars vars_to_free_vars variables x)

    in {parameters = active_vars vars_to_free_vars variables x,
        root_mean_squared_error = rms_of_error (P.distance quotes prices),
        quoted_prices = quotes,
        calibrated_prices = prices,
        nb_feval = nb_feval}
}

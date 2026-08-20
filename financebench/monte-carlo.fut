-- Monte Carlo option pricing engine.
-- Ported from FinanceBench (cavazos-lab/FinanceBench).
--
-- Prices a European put option using path-based Monte Carlo simulation
-- with geometric Brownian motion (GBM). Each sample simulates a 250-step
-- price path; the payoff is max(K - S(T), 0) * discount.
--
-- The reference implementation uses rand() for random numbers.
-- Here we use a simple LCG (linear congruential generator) seeded
-- per-sample to make the benchmark deterministic and parallelisable.
--
-- ==
-- entry: main
-- compiled input { 400000i64 }
-- output { 8.09363365f32 }

-- Constants from monteCarloConstants.h
def risk_val : f32 = 0.06f32
def div_val : f32 = 0.0f32
def volt_val : f32 = 0.200f32
def underlying_val : f32 = 30.0f32
def strike_val : f32 = 40.0f32
def discount_val : f32 = 0.94176453358424872f32
def sequence_length : i64 = 250

-- Inverse normal CDF approximation (Beasley-Springer-Moro).
-- Valid for x in (0, 1).
def inv_norm_cdf (x: f32) : f32 =
  let a1 = -39.696830286653757f32
  let a2 = 220.94609842452050f32
  let a3 = -275.92851044696869f32
  let a4 = 138.35775186726900f32
  let a5 = -30.664798066147160f32
  let a6 = 2.5066282774592392f32
  let b1 = -54.476098798224058f32
  let b2 = 161.58583685804089f32
  let b3 = -155.69897985988661f32
  let b4 = 66.801311887719720f32
  let b5 = -13.280681552885721f32
  let z = x - 0.5f32
  let r = z * z
  in (((((a1*r+a2)*r+a3)*r+a4)*r+a5)*r+a6) * z
     / (((((b1*r+b2)*r+b3)*r+b4)*r+b5)*r+1.0f32)

-- Forward rate calculation.
def forward_rate (rate: f32) (t1: f32) (t2: f32) : f32 =
  let disc1 = 1.0f32 / f32.exp (rate * t1)
  let disc2 = 1.0f32 / f32.exp (rate * t2)
  let compound = disc1 / disc2
  in f32.log compound / (t2 - t1)

-- Drift of the GBM process.
def process_drift (t: f32) : f32 =
  let epsilon = 0.0001f32
  let t1 = t + epsilon
  let sigma = volt_val
  in forward_rate risk_val t t1
     - forward_rate div_val t t1
     - 0.5f32 * sigma * sigma

-- Simple LCG for deterministic parallel random numbers.
-- Returns a value in (0, 1).
def lcg_next (state: u32) : (u32, f32) =
  let new_state = state * 1664525u32 + 1013904223u32
  let v = f32.u32 (new_state >> 1) / 2147483648.0f32
  -- Clamp to (0,1) to avoid edge cases in inverse CDF
  let v = f32.max 0.001f32 (f32.min 0.999f32 v)
  in (new_state, v)

-- Simulate one Monte Carlo path and return the discounted payoff.
def mc_path (seed: u32) : f32 =
  let dt = 1.0f32 / f32.i64 sequence_length
  let sqrt_dt = f32.sqrt dt
  -- Evolve path: S(t+dt) = S(t) * exp(drift*dt + sigma*sqrt(dt)*z)
  let (_state, s_final) =
    loop (state, s) = (seed, underlying_val) for i < sequence_length do
      let t = f32.i64 (i + 1) * dt
      let (state', u) = lcg_next state
      let z = inv_norm_cdf u
      let drift = process_drift t
      let dx = drift * dt + volt_val * sqrt_dt * z
      in (state', s * f32.exp dx)
  -- Put payoff
  in f32.max (strike_val - s_final) 0.0f32 * discount_val

-- Entry point: run num_samples Monte Carlo paths and return average price.
entry main (num_samples: i64) : f32 =
  let prices =
    map (\i -> mc_path (u32.i64 (i + 1) * 2654435761u32))
        (iota num_samples)
  in f32.sum prices / f32.i64 num_samples

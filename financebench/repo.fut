-- Repurchase agreement (repo) pricing engine.
-- Ported from FinanceBench (cavazos-lab/FinanceBench).
--
-- A repo is a short-term collateralised lending instrument where a bond is
-- sold with agreement to repurchase at a higher price. This computes the
-- repo forward value: the bond's dirty price adjusted for coupons received
-- during the repo period, compounded at the repo rate.
--
-- Formula:
--   bondFwd = (dirtyPrice - forwardSpotIncome) * compoundFactor(repoRate, t)
--
-- ==
-- entry: main
-- compiled input { 1000000i64 }

-- Discount factor using compound interest.
def discount_factor (rate: f32) (freq: f32) (t: f32) : f32 =
  1.0f32 / ((1.0f32 + rate / freq) ** (freq * t))

-- Compound factor using simple interest (for repo rate).
def simple_compound_factor (rate: f32) (t: f32) : f32 =
  1.0f32 + rate * t

-- Compute dirty price of a fixed-rate coupon bond.
def bond_dirty_price (rate: f32)
                     (freq: f32)
                     (yield_rate: f32)
                     (num_periods: i32)
                     (face_value: f32) : f32 =
  let coupon = face_value * rate / freq
  let coupon_pv =
    loop pv = 0.0f32
    for i < num_periods do
      let t = f32.i32 (i + 1) / freq
      in pv + coupon * discount_factor yield_rate freq t
  let t_maturity = f32.i32 num_periods / freq
  let face_pv = face_value * discount_factor yield_rate freq t_maturity
  in coupon_pv + face_pv

-- Forward spot income: PV of coupons received between settlement and delivery.
-- Simplified: assumes a fraction of the coupons fall within the repo period.
def forward_spot_income (rate: f32)
                        (freq: f32)
                        (face_value: f32)
                        (repo_term: f32)
                        (yield_rate: f32) : f32 =
  let coupon = face_value * rate / freq
  let period = 1.0f32 / freq
  -- Number of coupon payments within repo term
  let n_coupons = i32.f32 (repo_term * freq)
  in loop income = 0.0f32
     for i < n_coupons do
       let t = f32.i32 (i + 1) * period
       in income + coupon * discount_factor yield_rate freq t

-- Price a single repo: compute the bond forward value.
def repo_forward_value (bond_rate: f32)
                       (yield_rate: f32)
                       (repo_rate: f32)
                       (num_periods: i32)
                       (repo_term: f32) : f32 =
  let freq = 2.0f32
  let face_value = 100.0f32
  let dirty_price = bond_dirty_price bond_rate freq yield_rate num_periods face_value
  let spot_income = forward_spot_income bond_rate freq face_value repo_term yield_rate
  let compound = simple_compound_factor repo_rate repo_term
  in (dirty_price - spot_income) * compound

-- Simple LCG for deterministic random input generation.
def lcg (state: u32) : (u32, i32) =
  let new_state = state * 1103515245u32 + 12345u32
  in (new_state, i32.u32 ((new_state >> 16) & 0x7fffu32))

-- Generate repo parameters deterministically from index.
def gen_repo_params (idx: i64) : (f32, f32, f32, i32, f32) =
  let state = u32.i64 (idx + 1) * 2246822519u32
  let (state, v1) = lcg state
  let (state, v2) = lcg state
  let (state, v3) = lcg state
  let (_state, v4) = lcg state
  -- Bond rate: 0.03 to 0.13
  let bond_rate = 0.03f32 + 0.10f32 * f32.i32 (v1 % 1000) / 1000.0f32
  -- Yield rate: 0.05 to 0.09
  let yield_rate = 0.05f32 + 0.04f32 * f32.i32 (v2 % 1000) / 1000.0f32
  -- Repo rate: 0.00 to 0.10
  let repo_rate = 0.10f32 * f32.i32 (v3 % 1000) / 1000.0f32
  -- Number of bond coupon periods: 4 to 12
  let num_periods = 4 + v4 % 9
  -- Repo term: approximately 6-12 months (0.5 to 1.0 years)
  let repo_term = 0.5f32 + 0.5f32 * f32.i32 (v1 % 100) / 100.0f32
  in (bond_rate, yield_rate, repo_rate, num_periods, repo_term)

-- Entry point: price n repos in parallel.
-- Returns the bond forward value for each repo.
entry main (n: i64) : [n]f32 =
  map (\i ->
         let (bond_rate, yield_rate, repo_rate, num_periods, repo_term) = gen_repo_params i
         in repo_forward_value bond_rate yield_rate repo_rate num_periods repo_term)
      (iota n)

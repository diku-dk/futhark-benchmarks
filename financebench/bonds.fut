-- Bond pricing engine.
-- Ported from FinanceBench (cavazos-lab/FinanceBench).
--
-- Computes the dirty price of fixed-rate coupon bonds using discounted
-- cash flows (DCF). Each bond has semi-annual coupons. The discount
-- factor uses compound interest: discount(t) = 1 / (1 + r/freq)^(freq*t).
--
-- Input: arrays of bond parameters (n bonds computed in parallel).
-- Output: array of dirty prices.
--
-- ==
-- entry: main
-- compiled input { 1000000i64 }

-- Day count using serial date numbers (simplified 30/360 convention).
-- Days between two dates in (month, day, year) format using 30/360.
def days_between (m1: i32) (d1: i32) (y1: i32) (m2: i32) (d2: i32) (y2: i32) : f32 =
  let days = 360 * (y2 - y1) + 30 * (m2 - m1) + (d2 - d1)
  in f32.i32 days

-- Year fraction between two dates (30/360 convention).
def year_fraction (m1: i32) (d1: i32) (y1: i32) (m2: i32) (d2: i32) (y2: i32) : f32 =
  days_between m1 d1 y1 m2 d2 y2 / 360.0f32

-- Discount factor using compound interest.
-- discount(t) = 1 / (1 + rate/freq)^(freq * t)
def discount_factor (rate: f32) (freq: f32) (t: f32) : f32 =
  1.0f32 / ((1.0f32 + rate / freq) ** (freq * t))

-- Compute dirty price of a fixed-rate coupon bond.
-- Parameters:
--   rate: annual coupon rate
--   freq: coupon frequency (2 = semi-annual)
--   yield_rate: discount rate (yield to maturity)
--   num_periods: total number of coupon periods
--   face_value: par/face value of the bond
def bond_dirty_price (rate: f32) (freq: f32) (yield_rate: f32)
                     (num_periods: i32) (face_value: f32) : f32 =
  let coupon = face_value * rate / freq
  -- Sum discounted coupon cash flows
  let coupon_pv =
    loop pv = 0.0f32 for i < num_periods do
      let t = f32.i32 (i + 1) / freq
      in pv + coupon * discount_factor yield_rate freq t
  -- Add discounted face value at maturity
  let t_maturity = f32.i32 num_periods / freq
  let face_pv = face_value * discount_factor yield_rate freq t_maturity
  in coupon_pv + face_pv

-- Accrued interest for a bond.
-- Simplified: assumes we are at settlement which is some fraction through
-- the current coupon period.
def accrued_interest (rate: f32) (freq: f32) (face_value: f32)
                     (days_accrued: f32) (days_in_period: f32) : f32 =
  let coupon = face_value * rate / freq
  in coupon * days_accrued / days_in_period

-- Simple LCG for deterministic random input generation.
def lcg (state: u32) : (u32, i32) =
  let new_state = state * 1103515245u32 + 12345u32
  in (new_state, i32.u32 ((new_state >> 16) & 0x7fffu32))

-- Generate bond parameters deterministically from index (matching reference).
-- Bond issue date: day [1-28], month [1-12], year {1998,1999}
-- Bond maturity date: day [1-28], month [1-12], year {2000,2001}
-- Bond rate: random float between 0.03 and 0.13
def gen_bond_params (idx: i64) : (f32, f32, i32) =
  let state = u32.i64 (idx + 1) * 2654435761u32
  let (state, v1) = lcg state
  let (state, v2) = lcg state
  let (_state, v3) = lcg state
  -- Bond rate: 0.03 to 0.13
  let rate = 0.03f32 + 0.10f32 * f32.i32 (v1 % 1000) / 1000.0f32
  -- Number of coupon periods: 2 to 6 (semi-annual, 1-3 years maturity)
  let num_periods = 2 + v2 % 5
  -- Yield rate based on clean price of 89.97693786
  let yield_rate = 0.05f32 + 0.04f32 * f32.i32 (v3 % 1000) / 1000.0f32
  in (rate, yield_rate, num_periods)

-- Entry point: price n bonds in parallel.
-- Returns dirty prices, clean prices, accrued amounts, and forward values.
entry main (n: i64) : [n]f32 =
  map (\i ->
         let (rate, yield_rate, num_periods) = gen_bond_params i
         let freq = 2.0f32
         let face_value = 100.0f32
         in bond_dirty_price rate freq yield_rate num_periods face_value)
      (iota n)

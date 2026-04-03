-- | Implementations of HyperLogLog and HyperLogLog++
--
-- HyperLogLog [1] and HyperLogLog++ [2] are algorithms which solves
-- the count distinct elements problem in a probabilistic manner. They
-- are Monte Carlo algorithms so they give an estimate how many
-- distinct elements occur in the input array. The implementation of
-- HyperLogLog should be true to the original description but the
-- HyperLogLog++ does not utilize sparsity and is not as memory
-- efficient as it should be. The implementations are also not
-- thoroughly tested so use at own discretion.
--
-- [1] Philippe Flajolet, Éric Fusy, Olivier Gandouet, Frédéric
-- Meunier. HyperLogLog: the analysis of a near-optimal cardinality
-- estimation algorithm. AofA: Analysis of Algorithms, Jun 2007, Juan
-- les Pins, France. pp.137-156,
-- ⟨10.46298/dmtcs.3545⟩. ⟨hal-00406166v2⟩
--
-- [2] Stefan Heule, Marc Nunkesser, and Alexander
-- Hall. 2013. HyperLogLog in practice: algorithmic engineering of a
-- state of the art cardinality estimation algorithm. In Proceedings
-- of the 16th International Conference on Extending Database
-- Technology (EDBT '13). Association for Computing Machinery, New
-- York, NY, USA, 683–692. https://doi.org/10.1145/2452376.2452456

import "hyperloglog_data"
import "hashkey"
import "key"

module type hyperloglog = {
  -- | Type of elements to be counted.
  type t

  -- | Context to hash keys, see the hash key library for further
  -- elaboration.
  type~ ctx

  -- | The distinct element counting structure.
  type hyperloglog [m]

  --  | Create the distinct element counting structure.
  val create : (b: i64) -> hyperloglog [2 ** b]

  -- | Insert elements into the distinct element counting structure.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(1)*
  val insert [n] [b] :
    ctx
    -> *hyperloglog [2 ** b]
    -> [n]t
    -> *hyperloglog [2 ** b]

  -- | Estimate the number of distinct elments.
  --
  -- **Work:** *O(2^b)*
  --
  -- **Span:** *O(b)*
  val count [b] : hyperloglog [2 ** b] -> f64

  -- | Combines the count of distinct element.
  --
  -- **Work:** *O(2^b)*
  --
  -- **Span:** *O(1)*
  val merge [b] :
    hyperloglog [2 ** b]
    -> hyperloglog [2 ** b]
    -> hyperloglog [2 ** b]
}

module mk_hyperloglog
  (K: hashkey with hash = u32)
  : hyperloglog with t = K.key with ctx = K.ctx = {
  type t = K.key
  type~ ctx = K.ctx

  def const = K.rng_from_seed [123] |> K.rand |> (.1)

  type hyperloglog [m] =
    { registers: [m]i64
    , alpha: f64
    }

  def get_alpha (m: i64) : f64 =
    match m
    case 16 -> 0.673
    case 32 -> 0.697
    case 64 -> 0.709
    case _ -> 0.7213 / (1 + 1.079 / f64.i64 m)

  def create (b: i64) : hyperloglog [2 ** b] =
    assert (0 < b && b < i64.i32 u32.num_bits - 1)
    { registers = rep 0
    , alpha = get_alpha (2 ** b)
    }

  def get_rho (max_width: i64) (w: u32) : i64 =
    if w == 0
    then max_width + 1
    else let bit_length = i64.i32 u32.num_bits - i64.i32 (u32.clz w)
         in max_width - bit_length + 1

  def insert [n] [b]
             (ctx: ctx)
             ({registers, alpha}: *hyperloglog [2 ** b])
             (vs: [n]t) : *hyperloglog [2 ** b] =
    let hs = map (K.hash ctx const) vs
    let max_width = i64.i32 u32.num_bits - b
    let b_u32 = u32.i64 b
    let mask = (1 << b_u32) - 1
    let is = map (u32.to_i64 <-< (& mask)) hs
    let ws = map (>> b_u32) hs
    let ranks = map (get_rho max_width) ws
    let registers =
      reduce_by_index registers i64.max i64.lowest is ranks
    in {registers, alpha}

  def count [b] (hll: hyperloglog [2 ** b]) : f64 =
    let m = length hll.registers
    let m_f64 = f64.i64 m
    let indicator =
      map ((2 **) <-< f64.i64 <-< i64.neg) hll.registers
      |> f64.sum
    let raw_est = hll.alpha * m_f64 * m_f64 / indicator
    let v = map (i64.bool <-< (== 0)) hll.registers |> i64.sum
    let two_pow = 2.0 ** f64.i32 u32.num_bits
    let threshold = two_pow / 30.0
    in if v > 0
       then -- Small range correction (linear counting)
            m_f64 * f64.log (m_f64 / f64.i64 v)
       else if raw_est <= threshold
       then -- No correction needed
            raw_est
       else if raw_est < two_pow
       then -- Large range correction
            -two_pow * f64.log (1.0 - raw_est / two_pow)
       else -- Maximum cardinality
            two_pow - 1.0

  def merge [b]
            (hll: hyperloglog [2 ** b])
            (hll': hyperloglog [2 ** b]) : hyperloglog [2 ** b] =
    let merged_registers =
      map2 i64.max hll.registers hll'.registers
    in {registers = merged_registers, alpha = hll.alpha}
}

module mk_hyperloglog_plusplus
  (K: hashkey with hash = u64)
  : hyperloglog with t = K.key with ctx = K.ctx = {
  type t = K.key
  type~ ctx = K.ctx

  def const = K.rng_from_seed [123] |> K.rand |> (.1)

  type hyperloglog [m] =
    { registers: [m]i64
    , alpha: f64
    }

  def get_alpha (m: i64) : f64 =
    match m
    case 16 -> 0.673
    case 32 -> 0.697
    case 64 -> 0.709
    case _ -> 0.7213 / (1 + 1.079 / f64.i64 m)

  def create (p: i64) : hyperloglog [2 ** p] =
    assert (4 <= p && p <= 18)
    { registers = replicate (2 ** p) 0
    , alpha = get_alpha (2 ** p)
    }

  def insert [n] [p]
             (ctx: ctx)
             ({registers, alpha}: *hyperloglog [2 ** p])
             (vs: [n]t) : *hyperloglog [2 ** p] =
    let hs = map (K.hash ctx const) vs
    let p_u64 = u64.i64 p
    let is = map (\x -> i64.u64 (x >> (64 - p_u64))) hs
    let ws = map (\h -> (h << p_u64) | (1 << (p_u64 - 1))) hs
    let ranks = map (\w -> i64.i32 (u64.clz w) + 1) ws
    let registers =
      reduce_by_index registers i64.max i64.lowest is ranks
    in {registers, alpha}

  def estimate_bias (est: f64) (p: i64) : f64 =
    let offset = offsets[p - 4]
    let n = shape[p - 4]
    in if n == 0 || raw_estimate_data[offset] == 0.0
       then 0.0
       else if est <= raw_estimate_data[offset]
       then bias_data[offset]
       else if est >= raw_estimate_data[offset + n - 1]
       then bias_data[offset + n - 1]
       else let i =
              loop i = 1 while i < n && raw_estimate_data[offset + i] < est do i + 1
            let e1 = raw_estimate_data[offset + i - 1]
            let e2 = raw_estimate_data[offset + i]
            let b1 = bias_data[offset + i - 1]
            let b2 = bias_data[offset + i]
            let c = (est - e1) / (e2 - e1)
            in b1 * (1.0 - c) + b2 * c

  def linear_counting (m: f64) (v: i64) : f64 =
    m * f64.log (m / f64.i64 v)

  def count_zeros [m] (registers: [m]i64) : i64 =
    map (i64.bool <-< (== 0)) registers |> i64.sum

  def calculate_estimate [m] (registers: [m]i64) (alpha: f64) : f64 =
    let m_f64 = f64.i64 (length registers)
    let indicator =
      map ((2 **) <-< f64.i64 <-< i64.neg) registers
      |> f64.sum
    in alpha * m_f64 * m_f64 / indicator

  def count [p] (hll: hyperloglog [2 ** p]) : f64 =
    let m = length hll.registers
    let m_f64 = f64.i64 m
    let est = calculate_estimate hll.registers hll.alpha
    let est =
      if est <= 5.0 * m_f64
      then est - estimate_bias est p
      else est
    let v = count_zeros hll.registers
    in if v != 0
       then let lc = linear_counting m_f64 v
            let threshold = f64.u64 threshold_data[p - 4]
            in if lc <= threshold
               then lc
               else est
       else est

  def merge [p]
            (hll: hyperloglog [2 ** p])
            (hll': hyperloglog [2 ** p]) : hyperloglog [2 ** p] =
    let merged_registers =
      map2 i64.max hll.registers hll'.registers
    in {registers = merged_registers, alpha = hll.alpha}
}

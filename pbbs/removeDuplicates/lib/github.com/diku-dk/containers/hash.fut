-- | Internal library used for universal hashing.

import "../cpprandom/random"

-- | Unsigned integral type to use for hashing of built in integers.
module type uint = {
  -- | Unsigned integral type.
  type t

  -- | Unsigned integral type that is smaller that t.
  type u

  -- | The number zero.
  val zero : t

  -- | The number one.
  val one : t

  -- | A mersenne prime that is greater than the maximum u value and
  -- can fit in t.
  val prime : t

  -- | Bitwise and with prime.
  val and_prime : t -> t

  -- | Addition.
  val (+) : t -> t -> t

  -- | Addition between t and u.
  val add_small : t -> u -> t

  -- | Multiplication with the precondition that t is less than the
  -- mersenne prime.
  val (*) : t -> t -> t

  -- | Multiplication between t and u with the precondition that t is
  -- less than the mersenne prime.
  val mul_small : t -> u -> t

  -- | Subtraction.
  val (-) : t -> t -> t

  -- | Greater than or equal to the mersenne prime.
  val geq_prime : t -> bool

  -- | Equality.
  val (==) : t -> t -> bool

  -- | Bitwise right shift by mersenne prime
  val shift_prime : t -> t

  -- | Converts u to t.
  val from_u : u -> t

  -- | Converts u to t.
  val to_u : t -> u

  -- | Number of u64 that encodes a t.
  val n : i64

  -- | Converts an array of u64 to t.
  val from_u64 : [n]u64 -> t

  -- | Converts t to an array of u64.
  val to_u64 : t -> [n]u64
}

-- | Unsigned 128-bit integer meant for hashing to unsigned 32-bit
-- integers.
module u128 : uint with u = u32 = {
  type t = {high: u64, low: u64}
  type u = u32

  #[inline]
  def zero =
    {high = 0u64, low = 0u64}

  #[inline]
  def one =
    {high = 0u64, low = 1u64}

  -- | 2 ** 61 - 1
  #[inline]
  def prime =
    {high = 0u64, low = 0x1FFFFFFFFFFFFFFFu64}

  #[inline]
  def low (a: t) =
    a.low

  #[inline]
  def high (a: t) =
    a.high

  #[inline]
  def and_prime (a: t) =
    {high = 0u64, low = low a & low prime}

  -- | Shift by 61.
  #[inline]
  def shift_prime (a: t) =
    { high = high a >> 61
    , low = (low a u64.>> 61) u64.| (high a u64.<< 3)
    }

  #[inline]
  def add_small (a: t) (b: u) =
    let lo = (low a) + u64.u32 b
    in { high = (high a) + u64.bool (lo < low a)
       , low = lo
       }

  #[inline]
  def (+) (a: t) (b: t) =
    let lo = (low a) + (low b)
    in { high = (high a) + (high b) + u64.bool (lo < low a)
       , low = lo
       }

  #[inline]
  def (-) (a: t) (b: t) =
    let lo = (low a) - (low b)
    in { high = (high a) - (high b) - u64.bool (lo > low a)
       , low = lo
       }

  #[inline]
  def geq_prime (a: t) : bool =
    (high a) != 0 || (low a) >= (low prime)

  #[inline]
  def (==) (a: t) (b: t) : bool =
    (high a) == (high b) && (low a) == (low b)

  def n : i64 = 2

  #[inline]
  def from_u64 (as: [n]u64) : t =
    {high = as[0], low = as[1]}

  #[inline]
  def from_u (a: u) : t =
    {high = 0, low = u64.u32 a}

  #[inline]
  def mul_small (a: t) (b: u) : t =
    let lo = u64.((low a) * (u32 b))
    let hi = u64.(mul_hi (low a) (u32 b))
    in {high = hi, low = lo}

  #[inline]
  def (*) (a: t) (b: t) : t =
    let lo = (low a) * (low b)
    let hi = u64.(mul_hi (low a) (low b))
    in {high = hi, low = lo}

  #[inline]
  def to_u (n: t) : u =
    u32.u64 n.low

  #[inline]
  def to_u64 (a: t) : [n]u64 =
    sized n [a.high, a.low]
}

type u128 = u128.t

-- | Unsigned 192-bit integer meant for hashing to unsigned 64-bit
-- integers.
module u192 : uint with u = u64 = {
  type t = {high: u64, mid: u64, low: u64}
  type u = u64

  #[inline]
  def zero =
    {high = 0u64, mid = 0u64, low = 0u64}

  #[inline]
  def one =
    {high = 0u64, mid = 0u64, low = 1u64}

  -- | 2 ** 89 - 1
  #[inline]
  def prime =
    { high = 0u64
    , mid = 0x1FFFFFFu64
    , low = 0xFFFFFFFFFFFFFFFFu64
    }

  #[inline]
  def low (a: t) =
    a.low

  #[inline]
  def mid (a: t) =
    a.mid

  #[inline]
  def high (a: t) =
    a.high

  #[inline]
  def and_prime (a: t) : t =
    {high = 0u64, mid = mid a & mid prime, low = low a & low prime}

  -- | Shift by 89
  #[inline]
  def shift_prime (a: t) =
    { high = 0u64
    , mid = high a >> 25
    , low = (mid a u64.>> 25) u64.| (high a u64.<< 39)
    }

  #[inline]
  def geq_prime (a: t) : bool =
    (high a) != 0
    || ((mid a) > (mid prime))
    || ((mid a) == (mid prime) && (low a) == (low prime))

  #[inline]
  def (==) (a: t) (b: t) : bool =
    (high a) == (high b) && (mid a) == (mid b) && (low a) == (low b)

  def n : i64 = 3

  #[inline]
  def from_u64 (as: [n]u64) : t =
    {high = as[0], mid = as[1], low = as[2]}

  #[inline]
  def from_u (a: u) : t =
    {high = 0u64, mid = 0u64, low = a}

  #[inline]
  def mul_small (a: t) (b: u) : t =
    let lo = (low a) * b
    let mi' = (mid a) * b
    let mi = mi' + u64.mul_hi (low a) b
    let carry = u64.bool (mi < mi')
    let hi = u64.mul_hi (mid a) b + carry
    in {high = hi, mid = mi, low = lo}

  #[inline]
  def (*) (a: t) (b: t) : t =
    let lo = (low a) * (low b)
    let mi'' = (mid a) * (low b)
    let mi' = mi'' + (mid b) * (low a)
    let mi = mi' + u64.mul_hi (low a) (low b)
    let carry = u64.bool (mi' < mi'') + u64.bool (mi < mi')
    let hi =
      (mid a) * (mid b)
      + u64.mul_hi (mid a) (low b)
      + u64.mul_hi (mid b) (low a)
      + carry
    in {high = hi, mid = mi, low = lo}

  #[inline]
  def add_small (a: t) (b: u) =
    let lo = (low a) + b
    in { high = high a
       , mid = (mid a) + u64.bool (lo < low a)
       , low = lo
       }

  #[inline]
  def (+) (a: t) (b: t) =
    let lo = (low a) + (low b)
    let mi' = (mid a) + (mid b)
    let mi = mi' + u64.bool (lo < low a)
    let hi = (high a) + (high b) + u64.bool (mi' < mid a) + u64.bool (mi < mi')
    in { high = hi
       , mid = mi
       , low = lo
       }

  #[inline]
  def (-) (a: t) (b: t) =
    let lo = (low a) - (low b)
    let mi' = (mid a) - (mid b)
    let mi = mi' - u64.bool (lo > low a)
    let hi = (high a) - (high b) - u64.bool (mi' > mid a) - u64.bool (mi > mi')
    in { high = hi
       , mid = mi
       , low = lo
       }

  #[inline]
  def to_u (n: t) : u =
    n.low

  #[inline]
  def to_u64 (a: t) : [n]u64 =
    sized n [a.high, a.mid, a.low]
}

type u192 = u192.t

-- | n-dimensional linear congruential generator. You can give it an
-- array of non-zero integers to generate an n-array of n random
-- numbers that will go in a full cycle such that every n-array of
-- numbers will be hit. The integer size is limited by the uint
-- implementation so the numbers will be betwen zero and less than the
-- mersenne prime. The purpose of the random number generator is
-- mainly meant for generating numbers for a universal hash function
-- as constants.
module mk_ndimlcg
  (U: uint)
  (P: {
    -- | The type of random numbers to generate and use for
    -- generation.
    type t

    -- | The dimension of the n-array to generate.
    val n : i64

    -- | Choose some non-zero constants to use for random number
    -- generation that must be less than the mersenne prime given in
    -- the module U.
    val mat : [n][n + 1]t
  }
  with t = U.t)
  : rng_engine with t = [P.n]U.t = {
  type t = [P.n]U.t
  type rng = [P.n]U.t

  #[inline]
  def add a b =
    U.(-- y = a + b < 2p
       let y = a + b
       -- y < p
       in if geq_prime y then y - prime else y)

  #[inline]
  def auxiliary (as: [P.n + 1]U.t) (xs: t) : U.t =
    U.(let z =
         tabulate P.n (\i ->
                         -- y < p^2
                         let y = as[i] * xs[i]
                         -- y < p + p^2 / 2^b < 2p
                         let y = and_prime y + shift_prime y
                         -- y < p
                         in if geq_prime y then y - prime else y)
         |> reduce_comm add U.zero
       -- z < 2p
       let z = z + as[P.n]
       -- z < p
       in if geq_prime z then z - prime else z)

  def rand (x: rng) : (rng, t) =
    let y = map2 auxiliary P.mat (rep x)
    in (y, y)

  def rng_from_seed [n] (seed: [n]i32) : rng =
    let y =
      (replicate U.n 0u64) with [0] = u64.sum (map u64.i32 seed)
    let x = U.from_u64 y
    in loop t = replicate P.n x
       for _i in 0..<10 do
         (rand t).0

  -- FIXME: Should allow for parallel creation.
  def split_rng (n: i64) (x: rng) : [n]rng =
    (.0)
    <| loop (dest, y) = (replicate n (replicate P.n U.zero), copy x)
       for i in 0..<n do
         let (y', _) = rand y
         in (dest with [i] = y', y')

  -- FIXME: This seems bad.
  def join_rng [n] (xs: [n]rng) : rng =
    xs[0]

  def min = replicate P.n U.zero
  def max = replicate P.n U.(prime - one)
}

-- | A module which contains universal hash functions. This module is
-- mainly meant for multiply modulo prime hash functions.
module type universal_hashing = {
  -- | The type of constants to be used in the universal hash
  -- function.
  type t

  -- | Type that is hashed to and from.
  type u

  -- | Hash a single value, where the first constant may not be zero.
  val hash : t -> t -> u -> u

  -- | Hash a fixed length vector of values, where all the first
  -- constants in the tuple may not be zero.
  val hash_vector [n] : [n](t, t) -> [n]u -> u

  -- | Hash a variable length string. It is given 3 constants, the
  -- first and third may not be zero. The function gets a an index and
  -- and the value that is being hashed, extracts the ith encoding of
  -- x as type u. Furthermore, the number of u that that encodes x
  -- is also given.
  val hash_string 'x : t -> t -> t -> (get: i64 -> x -> u) -> (num: i64) -> x -> u
}

module mk_universal_hashing
  (I: uint)
  (U: integral with t = I.u)
  : universal_hashing with t = I.t with u = I.u = {
  type t = I.t
  type u = I.u

  #[inline]
  def hash (a: t) (b: t) (x: u) : u =
    -- See this for further explanation:
    -- https://arxiv.org/pdf/2008.08654
    -- Let p = 2**b - 1 and p > u.
    I.(let p = prime
       -- y < 2p(u - 1) + (p - 1) < (2u - 1)p <= p**2
       let y = mul_small a x + b
       -- y < p + p**2 / 2**b < 2p
       let y = and_prime y + shift_prime y
       -- y < p
       let y = if geq_prime y then y - p else y
       in to_u y)

  #[inline]
  def hash_vector [n] (cs: [n](t, t)) (xs: [n]u) : u =
    -- Further reading:
    -- https://en.wikipedia.org/wiki/Universal_hashing#Hashing_vectors
    loop y = U.i64 0
    for i in 0..<n do
      y U.+ hash cs[i].0 cs[i].1 xs[i]

  #[inline]
  def hash_string 'x
                  (a: t)
                  (b: t)
                  (c: t)
                  (get: i64 -> x -> u)
                  (num: i64)
                  (x: x) : u =
    -- Further reading:
    -- https://en.wikipedia.org/wiki/Universal_hashing#Hashing_strings
    -- Let p = 2**b - 1 and p > u.
    I.(let p = prime
       let y =
         -- Invariant y < 2p
         loop y = from_u (get 0 x)
         for i in 1..<i64.max 1 num do
           -- y < 2p*p = 2p**2
           let y = y * c
           -- y < p + 2p**2 / 2**b < 3p
           let y = and_prime y + shift_prime y
           -- y < p + 3p / 2**b < p + 3
           let y = and_prime y + shift_prime y
           -- y < p + 3 + u < 2p
           in add_small y (get i x)
       -- y < p
       let y = if geq_prime y then y - p else y
       -- Use universal_hash
       -- y < p**2
       let y = a * y
       -- y < p + p**2 / 2**b < 2p
       let y = and_prime y + shift_prime y
       -- y < p
       let y = if geq_prime y then y - p else y
       -- y < 2p
       let y = y + b
       -- y < p
       let y = if geq_prime y then y - p else y
       in to_u y)
}

module universal_u32 = mk_universal_hashing u128 u32
module universal = mk_universal_hashing u192 u64

-- | A module which contains the FNV hash function.
module type fnv_hashing = {
  -- | Type that is hashed to and from.
  type u

  -- | Hash a single value using a get function which gets the nth byte..
  val hash 't : (get: i64 -> t -> u8) -> (num: i64) -> (x: t) -> u
}

module mk_fnv_hashing
  (I: integral)
  (C: {
    type u
    val offset_basis : u
    val prime : u
  }
  with u = I.t)
  : fnv_hashing with u = I.t = {
  type u = I.t

  #[inline]
  def hash 't
           (get: i64 -> t -> u8)
           (num: i64)
           (x: t) : u =
    loop h = C.offset_basis
    for i in 0..<num do
      let byte = get i x
      let h' = h I.^ I.u8 byte
      in h' I.* C.prime
}

module fnv_u32 = mk_fnv_hashing u32 {
  type u = u32
  def offset_basis : u32 = 2166136261
  def prime : u32 = 16777619
}

module fnv = mk_fnv_hashing u64 {
  type u = u64
  def offset_basis : u64 = 14695981039346656037
  def prime : u64 = 1099511628211
}

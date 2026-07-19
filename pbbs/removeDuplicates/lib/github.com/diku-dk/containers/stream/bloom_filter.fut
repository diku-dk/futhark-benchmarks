-- | Implementation of a Bloom filter.
--
-- A Bloom filter [1] is a probabilistic data structure for testing
-- set membership. It never produces false negatives, but may produce
-- false positives. It uses `k` hash functions each mapping elements
-- to one of `m` bit positions. Insertion sets all `k` bits, and a
-- membership query returns true if all `k` bits are set.
--
-- The false positive probability is approximately (1 - e**(-kn/m))**k
-- where n is the number of inserted elements. For a desired false
-- positive rate `p` and expected `n` elements, optimal choices [2]
-- are: m = -n * (ln p) / (ln 2)^2 and k = (m/n) * ln 2.
--
-- [1] Burton H. Bloom. 1970. Space/time trade-offs in hash coding with
-- allowable errors. Commun. ACM 13, 7 (July 1970), 422–426.
-- https://doi.org/10.1145/362686.362692
--
-- [2] Wikipedia contributors. (2026, May 27). Bloom filter. In
-- Wikipedia, The Free Encyclopedia. Retrieved 11:24, May 28, 2026,
-- from
-- https://en.wikipedia.org/w/index.php?title=Bloom_filter&oldid=1356312352

import "../core/hashkey"
import "../set/bitset"

module type bloom_filter = {
  -- | Type of elements to be stored.
  type t

  -- | Context to hash keys.
  type~ ctx

  -- | The Bloom filter with capacity `m` bits and `k` hash functions.
  type bloom [m] [k]

  -- | Create a Bloom filter.
  --
  -- `m` is the number of bit positions.
  -- `k` is the number of hash functions.
  --
  -- **Work:** *O(m + k)*
  --
  -- **Span:** *O(1)*
  val create : (m: i64) -> (k: i64) -> bloom [cap bitset.num_bits m] [k]

  -- | Insert elements into the Bloom filter.
  --
  -- **Work:** *O(n * k)*
  --
  -- **Span:** *O(k)* (likely *O(1)*)
  val insert [n] [m] [k] :
    ctx
    -> *bloom [cap bitset.num_bits m] [k]
    -> [n]t
    -> *bloom [cap bitset.num_bits m] [k]

  -- | Query membership of an element.
  --
  -- Returns false means the element is definitely not in the set.
  -- Returns true means the element is probably in the set.
  --
  -- **Work:** *O(k)*
  --
  -- **Span:** *O(1)*
  val member [m] [k] : ctx -> bloom [cap bitset.num_bits m] [k] -> t -> bool

  -- | Merge two Bloom filters with the same parameters.
  -- The result contains all elements of both filters (union).
  --
  -- **Work:** *O(m)*
  --
  -- **Span:** *O(1)*
  val merge [m] [k] :
    bloom [cap bitset.num_bits m] [k]
    -> bloom [cap bitset.num_bits m] [k]
    -> bloom [cap bitset.num_bits m] [k]

  -- | The number of bits set in the filter.
  --
  -- **Work:** *O(m)*
  --
  -- **Span:** *O(log m)*
  val size [m] [k] : bloom [cap bitset.num_bits m] [k] -> i64
}

module mk_bloom_filter
  (K: hashkey with hash = u64)
  : bloom_filter with t = K.key with ctx = K.ctx = {
  type t = K.key
  type~ ctx = K.ctx

  type bloom [m] [k] =
    { bits: bitset.bitset [m]
    , consts: [k]K.const
    }

  def hash64 (x: u64) : u64 =
    let x = (x ^ (x >> 30u64)) * 0xbf58476d1ce4e5b9u64
    let x = (x ^ (x >> 27u64)) * 0x94d049bb133111ebu64
    in x ^ (x >> 31u64)

  def create (m: i64) (k: i64) : bloom [cap bitset.num_bits m] [k] =
    let consts =
      tabulate k (\i ->
                    let h = i32.u64 (hash64 (u64.i64 (1 + i)))
                    in K.rng_from_seed [h] |> K.rand |> (.1))
    in { bits = bitset.empty m
       , consts = consts
       }

  def insert [n] [m] [k]
             (ctx: ctx)
             ({bits, consts}: *bloom [cap bitset.num_bits m] [k])
             (vs: [n]t) : *bloom [cap bitset.num_bits m] [k] =
    let k = length consts
    let is =
      tabulate_2d n k (\vi ki ->
                         i64.u64 (K.hash ctx consts[ki] vs[vi]) % m)
      |> flatten
    let bits = bitset.insert bits is
    in {bits, consts}

  def member [m] [k]
             (ctx: ctx)
             (bf: bloom [cap bitset.num_bits m] [k])
             (v: t) : bool =
    let k = length bf.consts
    in tabulate k (\ki ->
                     let i = i64.u64 (K.hash ctx bf.consts[ki] v) % m
                     in bitset.member i bf.bits)
       |> and

  def merge [m] [k]
            (a: bloom [cap bitset.num_bits m] [k])
            (b: bloom [cap bitset.num_bits m] [k]) : bloom [cap bitset.num_bits m] [k] =
    {bits = bitset.union a.bits b.bits, consts = a.consts}

  def size [m] [k] (bf: bloom [cap bitset.num_bits m] [k]) : i64 =
    bitset.size bf.bits
}

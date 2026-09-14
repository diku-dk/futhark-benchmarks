-- | Implementation of the Count-Min Sketch streaming algorithm.
--
-- Count-Min Sketch [1] is a probabilistic data structure for
-- estimating the frequency of elements in a data stream. It uses `d`
-- hash functions each mapping elements to one of `w` buckets, giving
-- a matrix of counters of shape `[d][w]`. Frequency queries return
-- the minimum counter across all rows, which gives an upper bound on
-- the true frequency.
--
-- The error in frequency estimates is within ε = e/w with probability
-- 1 - δ where δ = (1/2)**d. Typical choices are w = ceil(e/ε) and d =
-- ceil(log(1/δ)).
--
-- [1] Graham Cormode, S. Muthukrishnan, An improved data stream
-- summary: the count-min sketch and its applications, Journal of
-- Algorithms, Volume 55, Issue 1, 2005, Pages 58-75, ISSN 0196-6774,
-- https://doi.org/10.1016/j.jalgor.2003.12.001.
-- (https://www.sciencedirect.com/science/article/pii/S0196677403001913)

import "../core/hashkey"

module type count_min_sketch = {
  -- | Type of elements to be counted.
  type t

  -- | Context to hash keys, see the hash key library for further
  -- elaboration.
  type~ ctx

  -- | The Count-Min Sketch structure with `d` hash functions and `w`
  -- buckets.
  type sketch [d] [w]

  -- | Create a Count-Min Sketch.
  --
  -- `d` controls the probability of error: error probability ≤
  -- (1/2)^d.  `w` controls the magnitude of error: error ≤ e/w *
  -- total_count.
  --
  -- **Work:** *O(w * d)*
  --
  -- **Span:** *O(1)*
  val create : (d: i64) -> (w: i64) -> ?[d'][w'].sketch [d'] [w']

  -- | Insert elements into the sketch.
  --
  -- **Work:** *O(d * n)*
  --
  -- **Span:** *O(1)* (Assuming best case for reduce_by_index)
  val insert [n] [d] [w] :
    ctx
    -> *sketch [d] [w]
    -> [n]t
    -> *sketch [d] [w]

  -- | Estimate the frequency of an element.
  --
  -- Returns an upper bound on the true frequency. The estimate is
  -- never less than the true frequency.
  --
  -- **Work:** *O(d)*
  --
  -- **Span:** *O(log d)*
  val query [d] [w] : ctx -> sketch [d] [w] -> t -> i64

  -- | Merge two sketches created with the same constants.
  --
  -- **Work:** *O(d * w)*
  --
  -- **Span:** *O(1)*
  val merge [d] [w] :
    sketch [d] [w]
    -> sketch [d] [w]
    -> sketch [d] [w]

  -- | Total count of all inserted elements.
  --
  -- **Work:** *O(w)*
  --
  -- **Span:** *O(log w)*
  val total [d] [w] : sketch [d] [w] -> i64
}

module mk_count_min_sketch
  (K: hashkey with hash = u64)
  : count_min_sketch with t = K.key with ctx = K.ctx = {
  type t = K.key
  type~ ctx = K.ctx

  type sketch [d] [w] =
    { counts: [d][w]i64
    , consts: [d]K.const
    }

  def hash64 (x: u64) : u64 =
    let x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9u64
    let x = (x ^ (x >> 27)) * 0x94d049bb133111ebu64
    in x ^ (x >> 31)

  def create (d: i64) (w: i64) : ?[d'][w'].sketch [d'] [w'] =
    let consts =
      tabulate d (\i ->
                    let h = i32.u64 (hash64 (u64.i64 (1 + i)))
                    in K.rng_from_seed [h] |> K.rand |> (.1))
    in { counts = replicate d (replicate w 0)
       , consts = consts
       }

  def insert [n] [d] [w]
             (ctx: ctx)
             ({counts, consts}: *sketch [d] [w])
             (vs: [n]t) : *sketch [d] [w] =
    let is =
      tabulate_2d d n (\row col -> (row, i64.u64 (K.hash ctx consts[row] vs[col]) % w))
    let counts =
      reduce_by_index_2d counts (+) 0 (flatten is) (replicate (d * n) 1)
    in {counts, consts}

  def query [d] [w]
            (ctx: ctx)
            (sketch: sketch [d] [w])
            (v: t) : i64 =
    map2 (\c row ->
            let i = i64.u64 (K.hash ctx c v) % w
            in row[i])
         sketch.consts
         sketch.counts
    |> i64.minimum

  def merge [d] [w]
            (s: sketch [d] [w])
            (s': sketch [d] [w]) : sketch [d] [w] =
    s with counts = map2 (map2 (+)) s.counts s'.counts

  def total [d] [w] (s: sketch [d] [w]) : i64 =
    s.counts[0] |> i64.sum
}

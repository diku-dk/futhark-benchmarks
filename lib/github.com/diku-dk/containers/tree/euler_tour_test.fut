-- | ignore

-- ==
-- entry: euler_tour
-- input { [0, 0, 0] }
-- output { [0, 1, 3] [5, 2, 4] }
-- input { [2, 2, 2] }
-- output { [1, 3, 0] [2, 4, 5] }
-- input { [2, 0, 2] }
-- output { [1, 2, 0] [4, 3, 5] }

-- ==
-- property: prop_euler_tour

module E = import "euler_tour"
import "../../cpprandom/random"

-- The fiddling with integer types is solely to
-- make it easier to write test data.
entry euler_tour P =
  P
  |> map i64.i32
  |> E.euler_tour
  |> map (\(x, y) -> (i32.i64 x, i32.i64 y))
  |> unzip

module rng = minstd_rand
module dist = uniform_int_distribution i64 minstd_rand

-- XXX: the root is always the first element, and the parent vector has a very
-- particular structure. It might be a good idea to shuffle this.
entry random_P (n: i64) (seed: u64) =
  let rngs = rng.split_rng n (rng.rng_from_seed [i32.u64 (seed << 32), i32.u64 seed])
  in map2 (\i rng ->
             if i == 0
             then 0
             else let (_, parent) = dist.rand (0, i - 1) rng
                  in parent)
          (indices rngs)
          rngs

#[prop(gen(random_P))]
entry prop_euler_tour [n] (P: [n]i64) : bool =
  let tour = E.euler_tour P
  let endpoints =
    flatten (map (\(l, r) -> [l, r]) tour)
  let counts =
    hist (+) 0 (2 * n) endpoints (replicate (n * 2) 1i64)
  let is_perm =
    all (\c -> c == 1) counts
  let range_ok =
    all (\(l, r) ->
           0 <= l
           && l < r
           && r < 2 * n)
        tour
  let nesting_ok =
    all (\v ->
           P[v] == v
           || let (lv, rv) = tour[v]
              let p = P[v]
              let (lp, rp) = tour[p]
              in lp < lv && rv < rp)
        (iota n)
  let root =
    head (filter (\v -> P[v] == v) (iota n))
  let root_ok =
    let (l, r) = tour[root]
    in l == 0 && r == 2 * n - 1
  in is_perm
     && range_ok
     && nesting_ok
     && root_ok

-- | ignore

import "shuffle"

module shuffle = mk_shuffle pcg32

-- ==
-- entry: test_shuffle
-- input { 10 } output { 45 true }
-- input { 1000 } output { 499500 true }
entry test_shuffle (n: i32) =
  let rng = pcg32.rng_from_seed [123, n, 42]
  let (_, xs) = shuffle.shuffle rng (iota n)
  let sorted = map2 (<=) xs[:n-1] xs[1:] |> and
  in (i32.sum xs, !sorted)

-- | ignore

import "hashset"
import "key"
import "hash"

module hashset = mk_hashset i64key

-- ==
-- entry: test_find_all
-- compiled random input { 1000000i64 }
-- output { true }
-- compiled random input { 0i64 }
-- output { true }
entry test_find_all n =
  let xs = iota n
  let s = hashset.from_array () xs
  in all (\x -> hashset.member () x s) xs

-- ==
-- entry: test_does_not_find
-- compiled random input { 1000000i64 }
-- output { true }
-- compiled random input { 0i64 }
-- output { true }
entry test_does_not_find n =
  let ys = iota n
  let s = hashset.from_array () ys
  let idxs = (n..<n + 1)
  in all (\x -> hashset.not_member () x s) idxs

-- ==
-- entry: test_find_all_dups
-- compiled random input { 10000i64 }
-- output { true }
entry test_find_all_dups n =
  let xs = iota n |> map (% 10)
  let s = hashset.from_array () xs
  in all (\x -> hashset.member () x s) xs

-- ==
-- entry: test_does_not_find_dups
-- compiled random input { 10000i64 }
-- output { true }
entry test_does_not_find_dups n =
  let ys = iota n |> map (% 10)
  let s = hashset.from_array () ys
  let idxs = (10..<n)
  in all (\x -> hashset.not_member () x s) idxs

-- ==
-- entry: test_dedup
-- compiled random input { 100000i64 }
-- output { true }
entry test_dedup n =
  let ys = iota n |> map (% 100)
  let s = hashset.from_array () ys
  let arr = hashset.to_array s
  in length arr == 100 && all (\a -> or (map (== a) (iota 100))) arr

-- ==
-- entry: test_insert
-- compiled random input { 1000i64 }
-- output { true }
entry test_insert n =
  let xs = iota n
  let h = hashset.from_array () xs
  let p = iota (2 * n) |> hashset.insert () h |> hashset.to_array
  in length p == n * 2

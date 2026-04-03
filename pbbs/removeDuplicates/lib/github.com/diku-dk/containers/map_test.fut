-- | ignore

import "map"
import "opt"
import "hash"

module mk_map_test (M: map with ctx = () with key = i64)
  : {
      val test_find_all : i64 -> bool
      val test_does_not_find : i64 -> bool
      val test_find_all_dups : i64 -> bool
      val test_does_not_find_dups : i64 -> bool
      val test_dedup : i64 -> bool
      val test_hist [m] : (xs: [m]i64) -> bool
      val test_map : i64 -> bool
      val test_update : i64 -> bool
      val test_insert : i64 -> bool
      val test_insert_hist : i64 -> bool
    } = {
  def test_find_all n =
    let vs = map (+ 10) (iota n)
    let xs = iota n
    let h = M.from_array () (zip xs vs)
    let v = map (from_opt (-1) <-< \x -> M.lookup () x h) xs
    in all (\x -> M.member () x h) xs
       && all (\x -> any (== x) v) vs

  def test_does_not_find n =
    let xs = iota n
    let h = M.from_array () (zip xs (iota n))
    let idxs = (n..<n + 1)
    let v = map (from_opt (-1) <-< \x -> M.lookup () x h) idxs
    in all (\x -> M.not_member () x h) idxs
       && all (== (-1)) v

  def test_find_all_dups n =
    let xs = iota n |> map (% 10)
    let h = M.from_array () (zip xs (iota n))
    in all (\x -> M.member () x h) xs

  def test_does_not_find_dups n =
    let m = 10
    let xs = iota n |> map (% m)
    let h = M.from_array () (zip xs (iota n))
    let idxs = (m..<n)
    in all (\x -> M.not_member () x h) idxs

  def test_dedup n =
    let m = 50
    let xs = iota n |> map (% m)
    let h = M.from_array () (zip xs (iota n))
    let arr = M.to_array h |> map (.0)
    in length arr == m
       && all (\a -> or (map (== a) (iota m))) arr

  def test_hist [m] (xs: [m]i64) =
    let n = 50
    let ks = map (% n) xs
    let h = M.from_array_hist () (+) 0 (zip ks (replicate m 1i64))
    let (is, vs) = M.to_array h |> unzip
    let res = scatter (replicate n 0i64) is vs
    let expected = hist (+) 0i64 n ks (replicate m 1i64)
    in and (map2 (==) res expected)

  def test_map n =
    let xs = iota n
    let h = M.from_array () (zip xs xs)
    let p = M.map (* 2) h |> M.to_array
    in all (\(k, v) -> k * 2 == v) p

  def test_update n =
    let xs = iota n
    let h = M.from_array () (zip xs xs)
    let p =
      zip xs (replicate n (-1))
      |> M.update h
      |> M.to_array
    in all ((== (-1)) <-< (.1)) p

  def test_insert n =
    let xs = iota n
    let h = M.from_array () (zip xs xs)
    let p =
      zip (iota (2 * n)) (replicate (2 * n) (-1))
      |> M.insert () h
      |> M.to_array
    in all ((== (-1)) <-< (.1)) p && (length p == n * 2)

  def test_insert_hist n =
    let xs = iota n
    let h = M.from_array () (zip xs (replicate n 1))
    let p =
      zip (iota (2 * n)) (replicate (2 * n) 1)
      |> M.insert_with () (+) 0 h
      |> M.to_array
      |> map (.1)
    in i64.sum p == n * 3
}

import "../cpprandom/random"
import "hashmap"
import "key"
import "eytzinger"
import "arraymap"
import "opt"

module hashmap = mk_hashmap i64key
module hashmap_tests = mk_map_test hashmap

module linhashmap = mk_linear_hashmap i64key
module linhashmap_tests = mk_map_test linhashmap

module arraymap = mk_arraymap i64key
module arraymap_tests = mk_map_test arraymap

module eytzinger = mk_eytzinger i64key
module eytzinger_tests = mk_map_test eytzinger

-- ==
-- entry: hashmap_find_all arraymap_find_all linhashmap_find_all eytzinger_find_all
-- compiled random input { 100000i64 }
-- output { true }
-- compiled random input { 0i64 }
-- output { true }
entry hashmap_find_all = hashmap_tests.test_find_all
entry linhashmap_find_all = linhashmap_tests.test_find_all
entry arraymap_find_all = arraymap_tests.test_find_all
entry eytzinger_find_all = eytzinger_tests.test_find_all

-- ==
-- entry: hashmap_does_not_find arraymap_does_not_find linhashmap_does_not_find eytzinger_does_not_find
-- compiled random input { 100000i64 }
-- output { true }
-- compiled random input { 0i64 }
-- output { true }
entry hashmap_does_not_find = hashmap_tests.test_does_not_find
entry linhashmap_does_not_find = linhashmap_tests.test_does_not_find
entry arraymap_does_not_find = arraymap_tests.test_does_not_find
entry eytzinger_does_not_find = eytzinger_tests.test_does_not_find

-- ==
-- entry: hashmap_find_all_dups arraymap_find_all_dups linhashmap_find_all_dups eytzinger_find_all_dups
-- compiled random input { 1000i64 }
-- output { true }
entry hashmap_find_all_dups = hashmap_tests.test_find_all_dups
entry linhashmap_find_all_dups = linhashmap_tests.test_find_all_dups
entry arraymap_find_all_dups = arraymap_tests.test_find_all_dups
entry eytzinger_find_all_dups = eytzinger_tests.test_find_all_dups

-- ==
-- entry: hashmap_does_not_find_dups arraymap_does_not_find_dups linhashmap_does_not_find_dups eytzinger_does_not_find_dups
-- compiled random input { 1000i64 }
-- output { true }
entry hashmap_does_not_find_dups = hashmap_tests.test_does_not_find_dups
entry linhashmap_does_not_find_dups = linhashmap_tests.test_does_not_find_dups
entry arraymap_does_not_find_dups = arraymap_tests.test_does_not_find_dups
entry eytzinger_does_not_find_dups = eytzinger_tests.test_does_not_find_dups

-- ==
-- entry: hashmap_dedup arraymap_dedup linhashmap_dedup eytzinger_dedup
-- compiled random input { 1000i64 }
-- output { true }
entry hashmap_dedup = hashmap_tests.test_dedup
entry linhashmap_dedup = linhashmap_tests.test_dedup
entry arraymap_dedup = arraymap_tests.test_dedup
entry eytzinger_dedup = eytzinger_tests.test_dedup

-- ==
-- entry: hashmap_hist arraymap_hist linhashmap_hist eytzinger_hist
-- compiled random input { [1000]i64 }
-- output { true }
entry hashmap_hist = hashmap_tests.test_hist
entry linhashmap_hist = linhashmap_tests.test_hist
entry arraymap_hist = arraymap_tests.test_hist
entry eytzinger_hist = eytzinger_tests.test_hist

-- ==
-- entry: hashmap_map arraymap_map linhashmap_map eytzinger_map
-- compiled random input { 1000i64 }
-- output { true }
entry hashmap_map = hashmap_tests.test_map
entry linhashmap_map = linhashmap_tests.test_map
entry arraymap_map = arraymap_tests.test_map
entry eytzinger_map = eytzinger_tests.test_map

-- ==
-- entry: hashmap_update arraymap_update linhashmap_update eytzinger_update
-- compiled random input { 1000i64 }
-- output { true }
entry hashmap_update = hashmap_tests.test_map
entry linhashmap_update = linhashmap_tests.test_map
entry arraymap_update = arraymap_tests.test_map
entry eytzinger_update = eytzinger_tests.test_map

-- ==
-- entry: hashmap_insert arraymap_insert linhashmap_insert eytzinger_insert
-- compiled random input { 1000i64 }
-- output { true }
entry hashmap_insert = hashmap_tests.test_insert
entry linhashmap_insert = linhashmap_tests.test_insert
entry arraymap_insert = arraymap_tests.test_insert
entry eytzinger_insert = eytzinger_tests.test_insert

-- ==
-- entry: hashmap_insert_hist arraymap_insert_hist linhashmap_insert_hist eytzinger_insert_hist
-- compiled random input { 1000i64 }
-- output { true }
entry hashmap_insert_hist = hashmap_tests.test_insert_hist
entry linhashmap_insert_hist = linhashmap_tests.test_insert_hist
entry arraymap_insert_hist = arraymap_tests.test_insert_hist
entry eytzinger_insert_hist = eytzinger_tests.test_insert_hist

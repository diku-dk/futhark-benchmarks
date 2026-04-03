-- | ignore

import "../sorts/radix_sort"
import "../segmented/segmented"
import "array"
import "key"
import "hash"

module array = mk_array_key i64key
def seed = i64key.rng_from_seed [1]

local
def count_occourences_sort [n] (arr: [n]i64) : [](i64, i64) =
  let sorted = radix_sort_int i64.num_bits i64.get_bit arr
  let flags =
    map (\i ->
           i == 0 || sorted[i - 1] != sorted[i])
        (iota n)
  let as = segmented_scan (+) 0 flags (replicate n 1) |> zip sorted
  let segment_ends = rotate 1 flags
  let segment_end_offsets = segment_ends |> map i64.bool |> scan (+) 0
  let num_segments = if n > 0 then last segment_end_offsets else 0
  let scratch = replicate num_segments (0, 0)
  let index i f = if f then i - 1 else -1
  in scatter scratch (map2 index segment_end_offsets segment_ends) as

local
def dedup_sort [n] (arr: [n]i64) : []i64 =
  let sorted = radix_sort_int i64.num_bits i64.get_bit arr
  let flags =
    map (\i ->
           i == 0 || sorted[i - 1] != sorted[i])
        (iota n)
  in zip flags sorted
     |> filter (.0)
     |> map (.1)

local
def count_occourences [n] (arr: [n]i64) : [](i64, i64) =
  replicate n 1
  |> zip arr
  |> array.reduce_by_key () seed (+) 0i64
  |> (.1)

-- ==
-- entry: test_reduce_by_key
-- compiled random input { [10][10000]i64 }
-- output { true }
-- compiled random input { [100][5]i64 }
-- output { true }
-- compiled random input { [1][0]i64 }
-- output { true }
entry test_reduce_by_key [n] [m] (arrs: [n][m]i64) : bool =
  all (\arr ->
         let arr = map (% 100) arr
         let sort_counts = count_occourences_sort arr
         let size = length sort_counts
         let sort_counts = sized size sort_counts
         let counts =
           count_occourences arr
           |> radix_sort_int_by_key (.0) i64.num_bits i64.get_bit
           |> sized size
         in map2 (==) sort_counts counts |> and)
      arrs

-- ==
-- entry: test_dedup
-- compiled random input { [10][10000]i64 }
-- output { true }
-- compiled random input { [100][5]i64 }
-- output { true }
-- compiled random input { [1][0]i64 }
-- output { true }
entry test_dedup [n] [m] (arrs: [n][m]i64) : bool =
  all (\arr ->
         let arr = map (% 100) arr
         let sort_dedups = dedup_sort arr
         let size = length sort_dedups
         let sort_dedups = sized size sort_dedups
         let counts =
           array.dedup () seed arr
           |> (.1)
           |> radix_sort_int i64.num_bits i64.get_bit
           |> sized size
         in map2 (==) sort_dedups counts |> and)
      arrs

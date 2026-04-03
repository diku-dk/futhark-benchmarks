import "lib/github.com/diku-dk/containers/array"
import "lib/github.com/diku-dk/containers/key"
import "lib/github.com/diku-dk/containers/slice"
import "lib/github.com/diku-dk/sorts/merge_sort"

module array_key_int = mk_array_key static_i32key
module encoder = mk_encoder_u8 u8
module slice_key = mk_static_slice_key static_u8key encoder
module array_key_str = mk_array_key slice_key

-- ==
-- entry: hash_dedup_int sort_dedup_int
-- "randomSeq_10M" input @ data/randomSeq_10M.in
-- output { 6321400i64 }
-- "randomSeq_100M" input @ data/randomSeq_100M.in
-- output { 63215740i64 }
-- "exptSeq_10M" input @ data/exptSeq_10M.in
-- output { 2293652i64 }
-- "exptSeq_100M" input @ data/exptSeq_100M.in
-- output { 20301712i64 }
entry hash_dedup_int [n] (arr: [n]i32) =
  array_key_int.dedup () () arr |> (.1) |> length

entry sort_dedup_int [n] (arr: [n]i32) =
  let sorted = merge_sort (<=) arr
  let flags = map2 (!=) (rotate 1 sorted) sorted
  in zip flags sorted
     |> filter (.0)
     |> length

entry mk_slices [n] [m] (strings: [n]u8, offsets: [m]i64, lengths: [m]i64) : ([n]u8, [m](slice.slice u8)) =
  let slices = map2 slice.mk offsets lengths
  in (strings, slices)

-- ==
-- entry: hash_dedup_str sort_dedup_str
-- "trigramSeq_10M" compiled script input { mk_slices ($loaddata "data/trigramSeq_10M.in") }
-- output { 1385694i64 }
-- "trigramSeq_100M" compiled script input { mk_slices ($loaddata "data/trigramSeq_100M.in") }
-- output { 9345882i64 }
entry hash_dedup_str [n] [m] (ctx: [n]u8) (arr: [m](slice.slice u8)) =
  array_key_str.dedup ctx () arr |> (.1) |> length

entry sort_dedup_str [n] [m] (ctx: [n]u8) (arr: [m](slice.slice u8)) =
  let sorted = merge_sort (\a b -> (ctx, a) slice_key.<= (ctx, b)) arr
  let flags = map2 (\a b -> not ((ctx, a) slice_key.== (ctx, b))) (rotate 1 sorted) sorted
  in zip flags sorted
     |> filter (.0)
     |> length

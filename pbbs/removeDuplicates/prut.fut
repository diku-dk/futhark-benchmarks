import "../../lib/github.com/diku-dk/containers/array/array"
import "../../lib/github.com/diku-dk/containers/core/key"
import "../../lib/github.com/diku-dk/containers/core/slice"
import "../../lib/github.com/diku-dk/sorts/merge_sort"

module array_key_int = mk_array_key static_i32key
module encoder = mk_encoder_u8 u8
module slice_key = mk_static_slice_key static_u8key encoder
module array_key_str = mk_array_key slice_key

entry sort_dedup_str [n] [m] (ctx: [n]u8) (arr: [m](slice.slice u8)) =
  let sorted = merge_sort (\a b -> #[sequential] (ctx, a) slice_key.<= (ctx, b)) arr
  let flags = map2 (\a b -> not ((ctx, a) slice_key.== (ctx, b))) (rotate 1 sorted) sorted
  in zip flags sorted
     |> filter (.0)
     |> length

-- | ignore

import "bubble_sort"

-- ==
-- entry: sort_i32
-- input { empty(i32) }
-- output { empty(i32) }
-- input { [5,4,3,2,1] }
-- output { [1,2,3,4,5] }
-- input { [5,4,3,3,2,1] }
-- output { [1,2,3,3,4,5] }

entry sort_i32 (xs: []i32) = bubble_sort (i32.<=) xs

-- ==
-- entry: sort_u16
-- input { [5u16,4u16,3u16,2u16,1u16] }
-- output { [1u16,2u16,3u16,4u16,5u16] }

entry sort_u16 (xs: []u16) = bubble_sort (u16.<=) xs

-- ==
-- entry: sort_f32
-- input { [5f32,4f32,3f32,2f32,1f32] }
-- output { [1f32,2f32,3f32,4f32,5f32] }

entry sort_f32 (xs: []f32) = bubble_sort (f32.<=) xs

-- ==
-- entry: sort_perm_i32
-- input { [5,4,3,2,1,0,-1,-2] }
-- output { [7, 6, 5, 4, 3, 2, 1, 0] }

entry sort_perm_i32 (xs: []i32) =
  zip xs (iota (length xs))
  |> bubble_sort_by_key (.1) (<=)
  |> map (.2)

-- ==
-- entry: sort_perm_f32
-- input { [5f32,4f32,3f32,2f32,1f32,0f32,-1f32,-2f32] }
-- output { [7, 6, 5, 4, 3, 2, 1, 0] }

entry sort_perm_f32 (xs: []f32) =
  zip xs (iota (length xs))
  |> bubble_sort_by_key (.1) (<=)
  |> map (.2)

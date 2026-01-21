import "blocked_partition"

-- ==
-- entry: test_blocked_partition
-- random input { [1000]i32 }
-- output { true }
entry test_blocked_partition [n] (xs: [n]i32) =
  let (a0, a1) = blocked_partition 256 (< 0) xs
  let (b0, b1) = partition (< 0) xs
  let l0 = length a0
  let l1 = length a1
  let a0 = sized l0 a0
  let b0 = sized l0 b0
  let a1 = sized l1 a1
  let b1 = sized l1 b1
  in (map2 (==) a0 b0 |> and) && (map2 (==) a1 b1 |> and)

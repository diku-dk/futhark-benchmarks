-- | ignore

import "list"

-- ==
-- entry: test_rev
-- input { [2] } output { [2] 2 2 }
-- input { [1,2,3] } output { [3,2,1] 3 1 }

entry test_rev (xs: []i32) =
  let l = xs |> list.from_array |> list.rev
  in ( list.to_array l
     , list.head l
     , list.last l
     )

-- ==
-- entry: test_sub
-- input { empty([0]i32) 0i64 } error: .
-- input { [1,2,3] 0i64 } output { 1 }
-- input { [1,2,3] 1i64 } output { 2 }
-- input { [1,2,3] 4i64 } error: .

entry test_sub (xs: []i32) i =
  xs |> list.from_array |> flip list.sub i

-- ==
-- entry: test_concat
-- input { [1,2,3] [4,5,6] } output { [1,2,3,4,5,6] 1 6 }
-- input { [1,2,3] empty([0]i32) } output { [1,2,3] 1 3 }
-- input { empty([0]i32) [4,5,6] } output { [4,5,6] 4 6 }
entry test_concat (xs: []i32) (ys: []i32) =
  let l = list.from_array xs list.++ list.from_array ys
  in ( list.to_array l
     , list.head l
     , list.last l
     )

-- ==
-- entry: test_scan
-- input { empty([0]i32) } output { empty([0]i32) }
-- input { [1,2,3] } output { [1,2,6] }
-- input { [0,1,2] } output { [0,0,0] }

entry test_scan (xs: []i32) =
  list.from_array xs |> list.scan (*) |> list.to_array

-- ==
-- entry: test_reduce
-- input { [1,2,3] } output { 6 }
-- input { [0,1,2] } output { 0 }

entry test_reduce (xs: []i32) =
  list.from_array xs |> list.reduce (*) 1

-- ==
-- entry: test_reduce_comm
-- input { [1,2,3] } output { 6 }
-- input { [0,1,2] } output { 0 }

entry test_reduce_comm (xs: []i32) =
  list.from_array xs |> list.rev |> list.reduce_comm (*) 1

-- | ignore

import "opt"

-- ==
-- entry: test_from_opt_none
-- input { 0i64 } output { 0i64 }
-- input { 1i64 } output { 1i64 }
-- input { 2i64 } output { 2i64 }
-- input { 3i64 } output { 3i64 }
entry test_from_opt_none (c: i64) : i64 =
  let n: opt i64 = #none
  in from_opt c n

-- ==
-- entry: test_from_opt_some
-- input { 0i64 } output { 0i64 }
-- input { 1i64 } output { 1i64 }
-- input { 2i64 } output { 2i64 }
-- input { 3i64 } output { 3i64 }
entry test_from_opt_some (c: i64) : i64 =
  let n: opt i64 = #some c
  in from_opt (-1) n

-- ==
-- entry: test_map_opt_none
-- input {  } output { -1i64 }
-- input {  } output { -1i64 }
-- input {  } output { -1i64 }
-- input {  } output { -1i64 }
entry test_map_opt_none : i64 =
  let n: opt i64 = #none
  in map_opt (* 2) n
     |> from_opt (-1)

-- ==
-- entry: test_map_opt_some
-- input { 0i64 } output { 0i64 }
-- input { 1i64 } output { 2i64 }
-- input { 2i64 } output { 4i64 }
-- input { 3i64 } output { 6i64 }
entry test_map_opt_some (c: i64) : i64 =
  let n: opt i64 = #some c
  in map_opt (* 2) n
     |> from_opt (-1)

-- ==
-- entry: test_equal_opt
-- input { 0i64 } output { true }
-- input { 1i64 } output { true }
-- input { 2i64 } output { true }
-- input { 3i64 } output { true }
entry test_equal_opt (c: i64) : bool =
  let n: opt i64 = #some c
  let m: opt i64 = #none
  let eq = equal_opt (i64.==)
  in eq n n && not (eq n m) && not (eq m n) && eq m m

def combine (a: (i64, i64)) (b: (i64, i64)) : (i64, i64) =
  (a.0, b.1)

def combine' = add_identity combine

-- ==
-- entry: test_add_identity
-- random input { [100]i64 [100]i64 } output { true }
entry test_add_identity (arr: []i64) (arr': []i64) =
  zip arr arr'
  |> map some
  |> reduce combine' #none
  |> is_some

-- ==
-- entry: test_first_some
-- input { [1,2,3] } output { 1 }
-- input { [-1,-2,-3] } output { 0 }
-- input { [1,-2,3] } output { 1 }
-- input { [-1,-2,3] } output { 3 }
entry test_first_some (xs: []i32) =
  xs
  |> map (\x : opt i32 -> if x > 0 then #some x else #none)
  |> first_some
  |> from_opt 0

-- | ignore

import "perm"

open perm

def eqvi [n] (x:[n]i64) (y:[n]i64) =
  map2 (i64.==) x y |> reduce (&&) true

entry test : bool =
  let p = id 5
  let p' = swap p 3 2  -- [0,1,3,2,4]
  let p'inv = inv p'
  let p1 = mk [3,2,1,0]
  in eqvi (permute p (iota 5)) (iota 5)
     && eqvi (permute p' (iota 5)) [0,1,3,2,4]
     && eqvi (permute (compose p'inv p') (iota 5)) (iota 5)
     && eqvi (permute p1 (iota 4)) [3,2,1,0]
     && eqvi (permute (add p1 p') (iota (4+5)) :> [9]i64) [3,2,1,0,4,5,7,6,8]
     && eqvi (permute (rev p') (iota 5)) [4,2,3,1,0]

-- ==
-- entry: test
-- input { } output { true }

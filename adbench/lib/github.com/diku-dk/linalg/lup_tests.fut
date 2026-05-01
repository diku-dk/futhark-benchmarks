-- | ignore

import "lup"
import "perm"

module lup64 = mk_lup f64

type t = lup64.t

entry test_lup = lup64.lup

-- ==
-- entry: test_lup
-- input { [[2f64,1,1,0],[4f64,3,3,1],[8f64,7,9,5],[6f64,7,9,8]] }
-- output { [[8f64, 7, 9, 5],[0.75, 1.75, 2.25, 4.25],[0.5, -0.2857142857142857, -0.8571428571428572, -0.2857142857142858],[0.25, -0.42857142857142855, 0.3333333333333334, 0.6666666666666667]] [2i64, 3, 1, 0] }

--                                [0,1,2,3]
-- row 0 exchanged with row 2     [2,1,0,3]
-- row 1 exchanged with row 3     [2,3,0,1]
-- row 2 exchanged with row 3     [2,3,1,0]
-- row 3 exchanged with row 3     [2,3,1,0]

entry test_solve [m] (a:*[m][m]t) (b:*[m]t) : [m]t =
  let (lu,p) = lup64.lup a
  let pb = perm.permute p b
  let y = lup64.forsolve lu pb
  let x = lup64.backsolve lu y
  in x

-- ==
-- entry: test_solve
-- input { [[2.0,-1.0,-2.0],[-4.0,6.0,3.0],[-4.0,-2.0,8.0]] [8.0,-27.0,4.0] }
-- output { [6.0,-2.0,3.0] }

entry test_ols [m] (a:*[m][m]t) (b:*[m]t) : [m]t = lup64.ols a b

-- ==
-- entry: test_ols
-- input { [[2.0,-1.0,-2.0],[-4.0,6.0,3.0],[-4.0,-2.0,8.0]] [8.0,-27.0,4.0] }
-- output { [6.0,-2.0,3.0] }
-- input { [[2.0]] [8.0] }
-- output { [4.0] }
-- input { [[0f64,5,7.333],[4f64,2,1],[2f64,7,9]] [3f64,8,3] }
-- output { [9.120493991989331,-21.917890520694296,15.353805073431268] }

import "linalg"
module linalg = mk_linalg f64

def veccomp 'a [m] (eq : a -> a -> bool) (xs:[m]a) (ys:[m]a) : bool =
  reduce (&&) true (map2 eq xs ys)

entry test_ols_eq [m] (a:*[m][m]t) (b:*[m]t) : bool =
  let xs = lup64.ols (copy a) (copy b)
  let rs = linalg.matvecmul_row a xs
  let eps = 0.00005
  let eq a b = f64.abs (a - b) < eps
  in veccomp eq rs b

-- ==
-- entry: test_ols_eq
-- input { [[2.0,-1.0,-2.0],[-4.0,6.0,3.0],[-4.0,-2.0,8.0]] [8.0,-27.0,4.0] }
-- output { true }
-- input { [[2.0]] [8.0] }
-- output { true }
-- random input { [5][5]f64 [5]f64 } output { true }
-- random input { [20][20]f64 [20]f64 } output { true }
-- input { [[0f64,5,7.333],[4f64,2,1],[2f64,7,9]] [3f64,8,3] }
-- output { true }

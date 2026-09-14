-- | ignore

import "nmf"
import "linalg"

module nmf64 = mk_nmf f64
module linalg64 = mk_linalg f64

def nmf = nmf64.nmf

def matsub = map2 (map2 (f64.-))
def matprod = map2 (map2 (f64.*))

def matmean [m][n] (xss: [m][n]f64) =
  (f64.sum (map f64.sum xss)) / f64.i64 (m*n)

--Tests NMF and checks whether the approximated matrice has a norm close to the original matrix
entry nmf_test [m][n] (A : [m][n]f64) (max_iter: i64) (tol: f64) (test_tol : f64) : bool =
  let (W,H,_) = nmf A (m+n) max_iter tol
  let A2 = linalg64.matmul W H
  in nmf64.frob_norm A - nmf64.frob_norm A2 < test_tol

-- Test the NMF function
-- ==
-- entry: nmf_test
-- input {
-- [[5.0, 8.0, 6.0],
-- [7.0,8.0,2.0],
-- [4.0,7.0,8.0]] 500i64 0.001 0.01}
-- output { true }
-- compiled input {
-- [[4.0, 1.0, 2.0, 2.0],
-- [1.0, 2.0, 0.0, 1.0],
-- [2.0, 0.0, 3.0, 2.0],
-- [2.0, 1.0, 2.0, 1.0]] 500i64 0.001 0.01}
-- output { true }
-- compiled input {
-- [[8.6, 6.8, 8.4, 4.1, 7.1, 6.2],
-- [1.8, 6.1, 4.2, 3.4, 2.0, 1.3],
-- [2.3, 0.3, 5.3, 4.3, 5.2, 4.5],
-- [6.6, 0.4, 1.0, 4.0, 7.7, 7.2],
-- [2.2, 0.9, 1.3, 0.3, 8.1, 2.5],
-- [6.1, 3.9, 0.5, 8.5, 8.3, 0.3]] 500i64 0.001 0.01}
-- output { true }
-- compiled input {
-- [[6.0,5.0,3.0,2.0,3.0,0.0],
--  [7.0,3.0,5.0,3.0,4.0,3.0],
--  [1.0,4.0,4.0,6.0,9.0,4.0],
--  [7.0,0.0,2.0,0.0,9.0,1.0],
--  [6.0,4.0,7.0,8.0,5.0,2.0],
--  [1.0,2.0,5.0,6.0,2.0,8.0]] 500i64 0.001 0.01}
-- output { true }

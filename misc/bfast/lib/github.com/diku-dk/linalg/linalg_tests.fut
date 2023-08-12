-- | ignore

import "linalg"

module linalg_f64 = mk_linalg f64

-- ==
-- entry: test_veczeros
-- input { 3i64 }
-- output { [0.0, 0.0, 0.0] }
entry test_veczeros = linalg_f64.veczeros

-- ==
-- entry: test_vecones
-- input { 3i64 }
-- output { [1.0, 1.0, 1.0] }
entry test_vecones = linalg_f64.vecones

-- ==
-- entry: test_linspace
-- input { -1.0 1.0 5i64 }
-- output { [-1.0, -0.5, 0.0, 0.5, 1.0] }
entry test_linspace = linalg_f64.linspace

-- ==
-- entry: test_eye
-- input { 2i64 }
-- output { [[1.0, 0.0], [0.0, 1.0]] }
entry test_eye = linalg_f64.eye

-- ==
-- entry: test_matzeros
-- input { 3i64 2i64 }
-- output { [[0.0, 0.0], [0.0, 0.0], [0.0, 0.0]] }
entry test_matzeros = linalg_f64.matzeros

-- ==
-- entry: test_matones
-- input { 3i64 2i64 }
-- output { [[1.0, 1.0], [1.0, 1.0], [1.0, 1.0]] }
entry test_matones = linalg_f64.matones

-- ==
-- entry: test_dotprod
-- input { [0f64]   [0f64] }
-- output { 0f64 }
-- input { [1f64,2f64,3f64] [4f64,5f64,6f64] }
-- output { 32f64 }
entry test_dotprod = linalg_f64.dotprod

-- ==
-- entry: test_outer
-- input { [1.0,2.0,3.0] [5.0,2.0,3.0] }
-- output { [[5.0, 2.0, 3.0],
--           [10.0, 4.0, 6.0],
--           [15.0, 6.0, 9.0]] }
entry test_outer = linalg_f64.outer

-- ==
-- entry: test_cross
-- input { [3.0,-3.0,1.0] [4.0,9.0,2.0] }
-- output { [-15.0,-2.0,39.0] }
entry test_cross = linalg_f64.cross

-- ==
-- entry: test_inv
-- input { [[0.0f64, 1.0f64], [1.0f64, 0.0f64]] }
-- output { [[0.0f64, 1.0f64], [1.0f64, 0.0f64]] }
-- input { [[1.0f64, 2.0f64, 1.0f64], [2.0f64, 1.0f64, 1.0f64], [1.0f64, 1.0f64, 2.0f64]] }
-- output { [[-0.25f64, 0.75f64, -0.25f64], [0.75f64, -0.25f64, -0.25f64], [-0.25f64, -0.25f64, 0.75f64]] }
entry test_inv [n] (A: [n][n]f64): [n][n]f64 =
  linalg_f64.inv A

-- ==
-- entry: test_matmul
-- input {
--   [ [1.0,2.0], [3.0,4.0] ]
--   [ [5.0,6.0], [7.0,8.0] ]
-- }
-- output { [[19.0, 22.0], [43.0, 50.0]] }
entry test_matmul = linalg_f64.matmul

-- ==
-- entry: test_kronecker
-- input { [[1.0,2.0,0.0],[0.0,-1.0,3.0]]
--         [[1.0,2.0,3.0],
--          [4.0,5.0,6.0],
--          [7.0,8.0,9.0],
--          [10.0,11.0,12.0]] }
-- output { [[1.0,2.0,3.0,2.0,4.0,6.0,0.0,0.0,0.0],
--           [4.0,5.0,6.0,8.0,10.0,12.0,0.0,0.0,0.0],
--           [7.0,8.0,9.0,14.0,16.0,18.0,0.0,0.0,0.0],
--           [10.0,11.0,12.0,20.0,22.0,24.0,0.0,0.0,0.0],
--           [0.0,0.0,0.0,-1.0,-2.0,-3.0,3.0,6.0,9.0],
--           [0.0,0.0,0.0,-4.0,-5.0,-6.0,12.0,15.0,18.0],
--           [0.0,0.0,0.0,-7.0,-8.0,-9.0,21.0,24.0,27.0],
--           [0.0,0.0,0.0,-10.0,-11.0,-12.0,30.0,33.0,36.0]] }
entry test_kronecker = linalg_f64.kronecker

-- ==
-- entry: test_ols
-- input {
--   [[-1f64, 0f64], [0.98f64, -0.2f64], [-0.32f64, -0.95f64], [-0.71f64, -0.71f64]]
--   [-1.77f64, 1.72f64, -2.41f64, -2.81f64]
-- }
-- output { [1.9734432f64, 1.8890195f64] }
entry test_ols = linalg_f64.ols

-- ==
-- entry: test_block
-- input {
--   [[0.0]]
--   [[1.0, 2.0]]
--   [[3.0], [6.0]]
--   [[4.0, 5.0], [7.0, 8.0]]
-- }
-- output {
--   [[0.0, 1.0, 2.0],
--    [3.0, 4.0, 5.0],
--    [6.0, 7.0, 8.0]]
-- }
entry test_block = linalg_f64.block

-- ==
-- entry: test_matunary_neg
-- input {
--   [[0.0, 1.0, 2.0],
--    [3.0, 4.0, 5.0],
--    [6.0, 7.0, 8.0]]
-- }
-- output {
--   [[-0.0, -1.0, -2.0],
--    [-3.0, -4.0, -5.0],
--    [-6.0, -7.0, -8.0]]
-- }
entry test_matunary_neg = linalg_f64.matunary f64.neg

-- ==
-- entry: test_matop_plus
-- input {
--   [[0.0, 1.0, 2.0],
--    [3.0, 4.0, 5.0],
--    [6.0, 7.0, 8.0]]
--
--   [[0.0, 1.0, 2.0],
--    [3.0, 4.0, 5.0],
--    [6.0, 7.0, 8.0]]
-- }
-- output {
--   [[0.0, 2.0, 4.0],
--    [6.0, 8.0, 10.0],
--    [12.0, 14.0, 16.0]]
-- }
entry test_matop_plus = linalg_f64.matop (+)

-- ==
-- entry: test_matcomp_lessthan
-- input {
--   [[0.0, 1.0, 2.0],
--    [3.0, 4.0, 5.0],
--    [6.0, 7.0, 8.0]]
--
--   [[3.0, 3.0, 3.0],
--    [3.0, 3.0, 3.0],
--    [3.0, 3.0, 3.0]]
-- }
-- output {
--   [[true, true, true],
--    [false, false, false],
--    [false, false, false]]
-- }
entry test_matcomp_lessthan = linalg_f64.matcomp (<)

-- ==
-- entry: test_vecscale
-- input {
--   10.0
--   [1.0, 2.0, 3.0]
-- }
-- output {
--   [10.0, 20.0, 30.0]
-- }
entry test_vecscale = linalg_f64.vecscale

-- ==
-- entry: test_matscale
-- input {
--   10.0
--   [[0.0, 1.0, 2.0],
--    [3.0, 4.0, 5.0],
--    [6.0, 7.0, 8.0]]
-- }
-- output {
--   [[0.0, 10.0, 20.0],
--    [30.0, 40.0, 50.0],
--    [60.0, 70.0, 80.0]]
-- }
entry test_matscale = linalg_f64.matscale

-- ==
-- entry: test_matsub
-- input {
--   [[0.0, 1.0, 2.0],
--    [3.0, 4.0, 5.0],
--    [6.0, 7.0, 8.0]]
--   [[0.0, 1.0, 2.0],
--    [3.0, 4.0, 5.0],
--    [6.0, 7.0, 8.0]]
-- }
-- output {
--   [[0.0, 0.0, 0.0],
--    [0.0, 0.0, 0.0],
--    [0.0, 0.0, 0.0]]
-- }
entry test_matsub = linalg_f64.matsub

-- ==
-- entry: test_matadd
-- input {
--   [[0.0, 1.0, 2.0],
--    [3.0, 4.0, 5.0],
--    [6.0, 7.0, 8.0]]
--   [[0.0, 1.0, 2.0],
--    [3.0, 4.0, 5.0],
--    [6.0, 7.0, 8.0]]
-- }
-- output {
--   [[0.0, 2.0, 4.0],
--    [6.0, 8.0, 10.0],
--    [12.0, 14.0, 16.0]]
-- }
entry test_matadd = linalg_f64.matadd

-- ==
-- entry: test_vecnorm
-- input {
--   [0.0, 3.0, 4.0]
-- }
-- output {
--   5.0
-- }
entry test_vecnorm = linalg_f64.vecnorm

-- ==
-- entry: test_matdiag
-- input {
--   [[0.0, 1.0, 2.0],
--    [3.0, 4.0, 5.0],
--    [6.0, 7.0, 8.0]]
-- }
-- output {
--   [[0.0, 0.0, 0.0],
--    [0.0, 4.0, 0.0],
--    [0.0, 0.0, 8.0]]
-- }
entry test_matdiag = linalg_f64.matdiag

-- ==
-- entry: test_todiag
-- input {
--   [0.0, 4.0, 8.0]
-- }
-- output {
--   [[0.0, 0.0, 0.0],
--    [0.0, 4.0, 0.0],
--    [0.0, 0.0, 8.0]]
-- }
entry test_todiag = linalg_f64.todiag

-- ==
-- entry: test_fromdiag
-- input {
--   [[0.0, 1.0, 2.0],
--    [3.0, 4.0, 5.0],
--    [6.0, 7.0, 8.0]]
-- }
-- output {
--   [0.0, 4.0, 8.0]
-- }
entry test_fromdiag = linalg_f64.fromdiag

-- ==
-- entry: test_househess_Q
-- input {
--   [[0.0, 1.0, 2.0],
--    [3.0, 4.0, 5.0],
--    [6.0, 7.0, 8.0]]
-- }
-- output {
--   [[ 1.0       ,  0.0       ,  0.0       ],
--    [ 0.0       , -0.4472136 , -0.89442719],
--    [ 0.0       , -0.89442719,  0.4472136 ]]
-- }
entry test_househess_Q = linalg_f64.househess >-> (.1)

-- ==
-- entry: test_househess_H
-- input {
--   [[0.0, 1.0, 2.0],
--    [3.0, 4.0, 5.0],
--    [6.0, 7.0, 8.0]]
-- }
-- output {
--   [[ 0.00000000e+00, -2.23606798e+00,  2.02860203e-16],
--    [-6.70820393e+00,  1.20000000e+01,  3.00000000e+00],
--    [ 0.00000000e+00,  1.00000000e+00, -1.55001191e-15]]
-- }
entry test_househess_H = linalg_f64.househess >-> (.0)

-- ==
-- entry: test_eig_D
-- input {
--   [[0.0, 1.0, 2.0],
--    [3.0, 4.0, 5.0],
--    [6.0, 7.0, 8.0]]
-- }
-- output {
--   [[1.33484692e+01, 0.0, 0.0],
--    [0.0, -1.34846923e+00, 0.0],
--    [0.0, 0.0, -1.15433316e-15]]
-- }
entry test_eig_D = linalg_f64.eig >-> (.0)

-- ==
-- entry: test_eig
-- input {
--   [[1.0, 0.0, 0.0],
--    [0.0, 2.0, 0.0],
--    [0.0, 0.0, 3.0]]
-- }
-- output {
--   true
-- }
-- input {
--   [[0.0, 1.0, 2.0],
--    [3.0, 4.0, 5.0],
--    [6.0, 7.0, 8.0]]
-- }
-- output {
--   true
-- }
entry test_eig X =
  let (D, V) = linalg_f64.eig X --decompose
  let X' = linalg_f64.(V `matmul` D `matmul` inv V) --recompose
  let eps = 1e-10
  let diff = linalg_f64.(X `matsub` X')
  in diff |> flatten |> all (\d -> d < eps) --check that the recomposition matches

-- ==
-- entry: test_matsqrt
-- input {
--   [[1.0, 1.0],
--    [2.0, 3.0]]
-- }
-- output {
--   [[0.81649658, 0.40824829],
--    [0.81649658, 1.63299316]]
-- }
entry test_matsqrt = linalg_f64.matsqrt






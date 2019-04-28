-- Simple cases with addition operator, which can be translated
-- directly to atomic addition.
-- ==
-- entry: sum_i32
-- random input { 10 [1000000]i32 [1000000]i32 } auto output
-- random input { 100 [1000000]i32 [1000000]i32 } auto output
-- random input { 1000 [1000000]i32 [1000000]i32 } auto output
-- random input { 10000 [1000000]i32 [1000000]i32 } auto output
-- random input { 100000 [1000000]i32 [1000000]i32 } auto output

entry sum_i32 [n] (k: i32) (is : [n]i32) (vs : [n]i32) : [k]i32 =
  reduce_by_index (replicate k 0) (+) 0 (map (%k) is) vs

-- An f32 requires a little more work from the compiler.
-- ==
-- entry: sum_f32
-- random input { 10 [1000000]i32 [1000000]f32 } auto output
-- random input { 100 [1000000]i32 [1000000]f32 } auto output
-- random input { 1000 [1000000]i32 [1000000]f32 } auto output
-- random input { 10000 [1000000]i32 [1000000]f32 } auto output
-- random input { 100000 [1000000]i32 [1000000]f32 } auto output

entry sum_f32 [n] (k: i32) (is : [n]i32) (vs : [n]f32) : [k]f32 =
  reduce_by_index (replicate k 0) (+) 0 (map (%k) is) vs

-- Do both!
-- ==
-- entry: sum_i32_f32
-- random input { 10 [1000000]i32 [1000000]i32 [1000000]f32 } auto output
-- random input { 100 [1000000]i32 [1000000]i32 [1000000]f32 } auto output
-- random input { 1000 [1000000]i32 [1000000]i32 [1000000]f32 } auto output
-- random input { 10000 [1000000]i32 [1000000]i32 [1000000]f32 } auto output
-- random input { 100000 [1000000]i32 [1000000]i32 [1000000]f32 } auto output

entry sum_i32_f32 [n] (k: i32) (is : [n]i32) (vs1 : [n]i32) (vs2 : [n]f32) : ([k]i32, [k]f32) =
  (reduce_by_index (replicate k 0) (+) 0 (map (%k) is) vs1,
   reduce_by_index (replicate k 0) (+) 0 (map (%k) is) vs2)

-- Now a fancier operator.  This is much slower because it will likely
-- involve locks and such, so use smaller input sizes.
-- ==
-- entry: absmax_i32
-- random input { 10 [1000000]i32 [1000000]i32 } auto output
-- random input { 100 [1000000]i32 [1000000]i32 } auto output
-- random input { 1000 [1000000]i32 [1000000]i32 } auto output
-- random input { 10000 [1000000]i32 [1000000]i32 } auto output
-- random input { 100000 [1000000]i32 [1000000]i32 } auto output

let absmax (x: i32) (y: i32): i32 =
  if i32.abs x < i32.abs y then y else x

entry absmax_i32 [n] (k: i32) (is : [n]i32) (vs : [n]i32) : [k]i32 =
  reduce_by_index (replicate k 0) absmax 0 (map (%k) is) vs

-- Now a vectorised operator.  If the compiler is clever, it can
-- compile this quite efficiently.
-- ==
-- entry: sum_vec_i32
-- random input { 10 [10000]i32 [1000000]i32 } auto output
-- random input { 10 [1000]i32 [1000000]i32 } auto output
-- random input { 10000 [10000]i32 [1000000]i32 } auto output
-- random input { 10000 [1000]i32 [1000000]i32 } auto output

entry sum_vec_i32 [n][m] (k: i32) (is : [m]i32) (vs : [n]i32) : [k][]i32 =
  let l = n/m
  let vs' = unflatten m l vs
  in reduce_by_index (replicate k (replicate l 0)) (map2 (+))
                     (replicate l 0) (map (%k) is) vs'

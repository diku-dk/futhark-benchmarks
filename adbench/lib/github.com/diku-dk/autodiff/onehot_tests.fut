import "onehot"

entry test_arr_pair n =
  unzip (map unzip (onehots (onehot.(arr (pair f64 i32))) : [][n](f64,i32)))

-- ==
-- entry: test_arr_pair
-- input { 3i64 }
-- output {
--     [[1.0, 0.0, 0.0],
--       [0.0, 0.0, 0.0],
--       [0.0, 1.0, 0.0],
--       [0.0, 0.0, 0.0],
--       [0.0, 0.0, 1.0],
--       [0.0, 0.0, 0.0]]
--      [[0, 0, 0],
--       [1, 0, 0],
--       [0, 0, 0],
--       [0, 1, 0],
--       [0, 0, 0],
--       [0, 0, 1]]
-- }

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

-- ==
-- entry: test_cycle
-- input { 3i64 }
-- output {
--    [1.0, 0.0, 0.0]
--    [[[0, 0],
--      [0, 0],
--      [0, 0]],
--     [[1, 0],
--      [1, 0],
--      [1, 0]],
--     [[0, 1],
--      [0, 1],
--      [0, 1]]]
-- }

entry test_cycle n : ([]f64,[][n][2]i32) =
  unzip (onehots (onehot.(pair f64 (cycle (arr i32)))))

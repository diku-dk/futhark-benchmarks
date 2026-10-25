-- Benchmarks for array copies that are neither contiguous nor transpositions,
-- and are therefore performed by the general copy kernel in the GPU backends.
--
-- In most of these, the source and destination agree on which dimension has
-- unit stride, and the copy should run at close to memory bandwidth. The
-- exception is 'transposed_slice', where they disagree.

-- ==
-- entry: slice_2d
-- random input { [8192][8193]i32 }
-- random input { [65536][1025]i32 }
-- random input { [1048576][65]i32 }
-- random input { [16777216][5]i32 }

-- ==
-- entry: slice_3d
-- random input { [256][256][1025]i32 }
-- random input { [65536][32][33]i32 }

-- ==
-- entry: strided_1d
-- random input { [134217728]i32 }

-- ==
-- entry: reversed_rows
-- random input { [8192][8192]i32 }
-- random input { [16777216][4]i32 }

-- ==
-- entry: transposed_slice
-- random input { [8192][8192]i32 }

entry slice_2d [n] [m] (xss: [n][m]i32) = copy xss[:, 1:]

entry slice_3d [n] [m] [k] (xsss: [n][m][k]i32) = copy xsss[:, :, 1:]

entry strided_1d (xs: []i32) = copy xs[::2]

entry reversed_rows [n] [m] (xss: [n][m]i32) = copy (map reverse xss)

entry transposed_slice [n] [m] (xss: [n][m]i32) = copy (transpose xss)[1:, 1:]

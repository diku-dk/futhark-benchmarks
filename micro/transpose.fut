-- ==
-- entry: transpose_i8
-- random input { 1 100000000 [100000000]i8 }
-- random input { 10 10000000 [100000000]i8 }
-- random input { 100 1000000 [100000000]i8 }
-- random input { 1000 100000 [100000000]i8 }
-- random input { 10000 10000 [100000000]i8 }
-- random input { 100000 1000 [100000000]i8 }
-- random input { 1000000 100 [100000000]i8 }
-- random input { 10000000 10 [100000000]i8 }
-- random input { 100000000 1 [100000000]i8 }

-- ==
-- entry: transpose_i32
-- random input { 1 100000000 [100000000]i32 }
-- random input { 10 10000000 [100000000]i32 }
-- random input { 100 1000000 [100000000]i32 }
-- random input { 1000 100000 [100000000]i32 }
-- random input { 10000 10000 [100000000]i32 }
-- random input { 100000 1000 [100000000]i32 }
-- random input { 1000000 100 [100000000]i32 }
-- random input { 10000000 10 [100000000]i32 }
-- random input { 100000000 1 [100000000]i32 }

-- ==
-- entry: transpose_i64
-- random input { 1 100000000 [100000000]i64 }
-- random input { 10 10000000 [100000000]i64 }
-- random input { 100 1000000 [100000000]i64 }
-- random input { 1000 100000 [100000000]i64 }
-- random input { 10000 10000 [100000000]i64 }
-- random input { 100000 1000 [100000000]i64 }
-- random input { 1000000 100 [100000000]i64 }
-- random input { 10000000 10 [100000000]i64 }
-- random input { 100000000 1 [100000000]i64 }

entry transpose_i8  n m xs: [][]i8  = unflatten n m xs |> transpose
entry transpose_i32 n m xs: [][]i32 = unflatten n m xs |> transpose
entry transpose_i64 n m xs: [][]i64 = unflatten n m xs |> transpose

-- ==
-- entry: map_transpose_i8
-- random input { 1 1 100000000 [100000000]i8 }
-- random input { 10 1 10000000 [100000000]i8 }
-- random input { 1000 1 100000 [100000000]i8 }
-- random input { 1 1000 100000 [100000000]i8 }
-- random input { 10 1000 10000 [100000000]i8 }
-- random input { 1000 1000 100 [100000000]i8 }
-- random input { 1 10000000 10 [100000000]i8 }
-- random input { 10 10000000 1 [100000000]i8 }
-- random input { 1000 100000 1 [100000000]i8 }

-- ==
-- entry: map_transpose_i32
-- random input { 1 1 100000000 [100000000]i32 }
-- random input { 10 1 10000000 [100000000]i32 }
-- random input { 1000 1 100000 [100000000]i32 }
-- random input { 1 1000 100000 [100000000]i32 }
-- random input { 10 1000 10000 [100000000]i32 }
-- random input { 1000 1000 100 [100000000]i32 }
-- random input { 1 10000000 10 [100000000]i32 }
-- random input { 10 10000000 1 [100000000]i32 }
-- random input { 1000 100000 1 [100000000]i32 }

-- ==
-- entry: map_transpose_i64
-- random input { 1 1 100000000 [100000000]i64 }
-- random input { 10 1 10000000 [100000000]i64 }
-- random input { 1000 1 100000 [100000000]i64 }
-- random input { 1 1000 100000 [100000000]i64 }
-- random input { 10 1000 10000 [100000000]i64 }
-- random input { 1000 1000 100 [100000000]i64 }
-- random input { 1 10000000 10 [100000000]i64 }
-- random input { 10 10000000 1 [100000000]i64 }
-- random input { 1000 100000 1 [100000000]i64 }

entry map_transpose_i8  k n m xs:  [][][]i8 = unflatten_3d k n m xs |> map transpose
entry map_transpose_i32 k n m xs: [][][]i32 = unflatten_3d k n m xs |> map transpose
entry map_transpose_i64 k n m xs: [][][]i64 = unflatten_3d k n m xs |> map transpose

-- Checking that intra-group-parallel code isn't picked when it's a very bad idea.

-- ==
-- entry: scan_reduce
-- compiled random input { [1000000][6]f32 } auto output
entry scan_reduce = map (scan (+) 0 >-> f32.sum)

-- Test the library scan_segm functions.
-- ==
-- input {
--   [False, True, False, False, True, True, False]
--   [0, 1, 2, 3, 4, 5, 6]
-- }
-- output {
--   [1, 4, 5]
-- }

include bfs_lib

fun []i32 main([n]bool bs, [n]i32 ns) =
  i32_filter(bs, ns)

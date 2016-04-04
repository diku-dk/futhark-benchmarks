-- Test the library scan_segm functions.
-- ==
-- tags { disable }
-- input {
--   [False, True, False, False, True, True, False]
--   [0, 1, 2, 3, 4, 5, 6]
-- }
-- output {
--   [1, 4, 5]
-- }

include bfs_lib

fun [i32] main([bool, n] bs, [i32, n] ns) =
  i32_filter(bs, ns)

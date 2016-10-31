-- Test the library scan_segm functions.
-- ==
-- tags { nobench }
-- input {
--   [false, true, false, false, true, true, false]
--   [0, 1, 2, 3, 4, 5, 6]
-- }
-- output {
--   [1, 4, 5]
-- }

include bfs_lib

fun main(bs: [n]bool, ns: [n]i32): []i32 =
  i32_filter bs ns

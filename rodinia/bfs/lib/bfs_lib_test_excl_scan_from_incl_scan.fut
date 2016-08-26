-- Test the library scan conversion functions.
-- ==
-- tags { nobench }
-- input {
--   [5, 3, -3, 1, 99]
-- }
-- output {
--   [0, 5, 3, -3, 1]
-- }

include bfs_lib

fun main(ns: [n]i32): []i32 =
  i32_excl_scan_from_incl_scan(ns, 0)

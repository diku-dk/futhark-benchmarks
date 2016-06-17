-- Test the library scan conversion functions.
-- ==
-- input {
--   [5, 3, -3, 1, 99]
-- }
-- output {
--   [0, 5, 3, -3, 1]
-- }

include bfs_lib

fun []i32 main([n]i32 ns) =
  i32_excl_scan_from_incl_scan(ns, 0)

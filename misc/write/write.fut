-- Benchmark the speed of write on different datasets.
-- ==
-- tags { disable }
--
-- input @ indices_iota_10000.in
-- output @ indices_iota_10000.out
--
-- input @ indices_shuffled_10000.in
-- output @ indices_shuffled_10000.out
--
-- input @ indices_iota_1000000.in
-- output @ indices_iota_1000000.out
--
-- input @ indices_shuffled_1000000.in
-- output @ indices_shuffled_1000000.out

fun [n]i32
  main([k]i32 indices,
       [k]i32 values,
       *[n]i32 array) =
  write(indices, values, array)

-- Benchmark the speed of write on different datasets.
-- ==
-- tags { notravis }
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

fun [i32, n]
  main([i32, k] indices,
       [i32, k] values,
       *[i32, n] array) =
  write(indices, values, array)

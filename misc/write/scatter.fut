-- Benchmark the speed of scatter on different datasets.
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

let main(indices: [#k]i32,
         values: [#k]i32,
         array: *[#n]i32): [#n]i32 =
  scatter indices values array

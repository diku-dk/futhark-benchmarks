-- Benchmark with larger datasets.
-- ==
--
-- tags { }
-- input @ data/radix_sort_10K.in
-- input @ data/radix_sort_100K.in
-- input @ data/radix_sort_1M.in

module R = import "radix_sort_blelloch"

def main [n] (xs: [n]u32) = R.split_radix_sort(xs, 32)

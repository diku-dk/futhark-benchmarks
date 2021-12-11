-- Benchmark with larger datasets.
-- ==
--
-- tags { }
-- input @ data/radix_sort_10K.in
-- input @ data/radix_sort_100K.in
-- input @ data/radix_sort_1M.in

module R = import "radix_sort"

def main (xs: []u32) = R.radix_sort xs

import "lib/github.com/diku-dk/sorts/radix_sort"

entry sort_f64 = radix_sort f64.num_bits f64.get_bit

entry sort_f64_pair =
  map (\p -> (p[0],p[1]))
      >-> radix_sort_by_key (.0) f64.num_bits f64.get_bit
      >-> map (\(x,y) -> [x,y])

-- ==
-- entry: sort_f64
-- input @ data/almostSortedSeq_100M.in
-- input @ data/exptSeq_100M.in
-- input @ data/randomSeq_100M.in

-- ==
-- entry: sort_f64_pair
-- input @ data/randomSeq_100M_double_pair_double.in

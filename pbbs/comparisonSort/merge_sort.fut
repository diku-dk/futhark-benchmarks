import "lib/github.com/diku-dk/sorts/merge_sort"

entry sort_f64 = merge_sort (f64.<=)

entry sort_f64_pair =
  map (\p -> (p[0],p[1]))
      >-> merge_sort_by_key (.0) (f64.<=)
      >-> map (\(x,y) -> [x,y])

-- ==
-- entry: sort_f64
-- input @ data/almostSortedSeq_100M.in
-- input @ data/exptSeq_100M.in
-- input @ data/randomSeq_100M.in

-- ==
-- entry: sort_f64_pair
-- input @ data/randomSeq_100M_double_pair_double.in

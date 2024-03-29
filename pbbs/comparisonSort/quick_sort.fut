import "lib/github.com/diku-dk/sorts/quick_sort"

entry sort_f64 = qsort (f64.<=)

entry sort_f64_pair =
  map (\p -> (p[0],p[1]))
      >-> qsort_by_key (.0) (f64.<=)
      >-> map (\(x,y) -> [x,y])

-- ==
-- entry: sort_f64
-- mem_16gb input @ data/almostSortedSeq_100M.in
-- mem_16gb input @ data/exptSeq_100M.in
-- mem_16gb input @ data/randomSeq_100M.in

-- ==
-- entry: sort_f64_pair
-- mem_16gb input @ data/randomSeq_100M_double_pair_double.in

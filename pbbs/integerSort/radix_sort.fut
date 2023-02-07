-- ==
-- entry: sort_i32
-- "randomSeq_100M_int" script input { (27i32, $loaddata "data/randomSeq_100M_int.in") }
-- "exptSeq_100M_int" script input { (27i32, $loaddata "data/exptSeq_100M_int.in") }

-- ==
-- entry: sort_i32_pair
-- "randomSeq_100M_int_pair_int" script input { (27i32, $loaddata "data/randomSeq_100M_int_pair_int.in") }
-- "randomSeq_100M_256_int_pair_int" script input { (8i32, $loaddata "data/randomSeq_100M_256_int_pair_int.in") }

import "lib/github.com/diku-dk/sorts/radix_sort"

entry sort_i32 b = radix_sort_int b i32.get_bit

entry sort_i32_pair b =
  map (\p -> (p[0],p[1]))
      >-> radix_sort_int_by_key (.0) b i32.get_bit
      >-> map (\(x,y) -> [x,y])

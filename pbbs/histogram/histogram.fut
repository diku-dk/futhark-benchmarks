-- ==
-- script input { (256i64, $loaddata "data/randomSeq_100M_256.in") }
-- script input { (100_000i64, $loaddata "data/randomSeq_100M_100K.in") }
-- script input { (100_000_000i64, $loaddata "data/randomSeq_100M.in") }
-- script input { (100_000_000i64, $loaddata "data/exptSeq_100M.in") }
-- script input { (100_000_000i64, $loaddata "data/almostEqualSeq_100M.in") }

def main m xs : [m]i32 = hist (+) 0 m (map i64.i32 xs) (map (const 1) xs)

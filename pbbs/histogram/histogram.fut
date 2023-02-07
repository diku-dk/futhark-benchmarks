-- ==
-- "randomSeq_100M_256" script input
-- { (256i64, $loaddata "data/randomSeq_100M_256.in") }
-- "randomSeq_100M_100K" script input
-- { (100_000i64, $loaddata "data/randomSeq_100M_100K.in") }
-- "randomSeq_100M" script input
-- { (100_000_000i64, $loaddata "data/randomSeq_100M.in") }
-- "exptSeq_100M" script input
-- { (100_000_000i64, $loaddata "data/exptSeq_100M.in") }
-- "almostEqualSeq_100M" script input
-- { (100_000_000i64, $loaddata "data/almostEqualSeq_100M.in") }

def main m xs : [m]i32 = hist (+) 0 m (map i64.i32 xs) (map (const 1) xs)

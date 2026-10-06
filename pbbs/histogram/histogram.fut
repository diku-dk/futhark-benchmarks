-- ==
-- "randomSeq_100M_256" script input
-- { (256i64, io.loadvalue "data/randomSeq_100M_256.in" : [100000000]i32) }
-- "randomSeq_100M_100K" script input
-- { (100_000i64, io.loadvalue "data/randomSeq_100M_100K.in" : [100000000]i32) }
-- "randomSeq_100M" script input
-- { (100_000_000i64, io.loadvalue "data/randomSeq_100M.in" : [100000000]i32) }
-- "exptSeq_100M" script input
-- { (100_000_000i64, io.loadvalue "data/exptSeq_100M.in" : [100000000]i32) }
-- "almostEqualSeq_100M" script input
-- { (100_000_000i64, io.loadvalue "data/almostEqualSeq_100M.in" : [100000000]i32) }

def main m xs : [m]i32 = hist (+) 0 m (map i64.i32 xs) (map (const 1) xs)

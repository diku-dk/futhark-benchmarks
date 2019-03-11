-- | Heston calibration with double-precision floats.
--
-- ==
-- compiled input @ data/1062_quotes.in
-- compiled input @ data/10000_quotes.in
-- compiled input @ data/100000_quotes.in

import "lib/github.com/diku-dk/cpprandom/random"
import "heston"

module heston64 = heston f64 minstd_rand

let main [num_quotes]
         (max_global: i32)
         (num_points: i32)
         (np: i32)
         (today: i32)
         (quotes_maturity: [num_quotes]i32)
         (quotes_strike: [num_quotes]f64)
         (quotes_quote: [num_quotes]f64) =
  heston64.heston max_global num_points np today quotes_maturity quotes_strike quotes_quote

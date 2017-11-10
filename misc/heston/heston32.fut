-- ==
-- compiled input @ data/1062_quotes.in
-- compiled input @ data/10000_quotes.in
-- compiled input @ data/100000_quotes.in

import "/futlib/random"
import "/futlib/math"
import "heston"

module heston32 = heston f32 minstd_rand

-- We still read the data sets as double precision, and initially
-- convert them to single.  This is included in measurements, but
-- takes a negligible amount of time.
let main [num_quotes]
         (max_global: i32)
         (num_points: i32)
         (np: i32)
         (today: i32)
         (quotes_maturity: [num_quotes]i32)
         (quotes_strike: [num_quotes]f64)
         (quotes_quote: [num_quotes]f64) =
  heston32.heston max_global num_points np today quotes_maturity
  (map f32.f64 quotes_strike) (map f32.f64 quotes_quote)

-- This file contains exotic scan cases, such as operators that involve arrays,
-- vectorised operators, or very costly map-functions.

-- This function tests a scan with a vectorised operator. The map part is just
-- to avoid interchange.
-- ==
-- entry: scan_vector
-- random input { 100000i64 1000i64 [100000000]i32 } auto output
-- random input { 1000000i64 100i64 [100000000]i32 } auto output
-- random input { 10000000i64 10i64 [100000000]i32 } auto output
-- random input { 100000000i64 1i64 [100000000]i32 } auto output
entry scan_vector (n: i64) (m: i64) (a: [n * m]i32) =
  -- The map is just to avoid interchange.
  scan (map2 (+)) (replicate m 0) (map (map (+ 3)) (unflatten a))

def vecadd [n] (xs: [n]i32) (ys: [n]i32) : [n]i32 =
  loop res = #[scratch] replicate n 0
  for i < n do
    res with [i] = xs[i] + ys[i]

-- Like scan_vector, but written in such a way that the compiler does not
-- recognise the operator as a map2.
-- ==
-- entry: scan_arr
-- random input { 100000i64 1000i64 [100000000]i32 } auto output
-- random input { 1000000i64 100i64 [100000000]i32 } auto output
-- random input { 10000000i64 10i64 [100000000]i32 } auto output
-- random input { 100000000i64 1i64 [100000000]i32 } auto output
entry scan_arr (n: i64) (m: i64) (a: [n * m]i32) =
  scan vecadd (replicate m 0) (map (map (+ 3)) (unflatten a))

-- A scan where the work of the map function far outstrips (for some datasets)
-- the cost of actually doing the scan.
-- ==
-- entry: scan_fanin
-- random input { 100000i64 1000i64 [100000000]i32 } auto output
-- random input { 1000000i64 100i64 [100000000]i32 } auto output
-- random input { 10000000i64 10i64 [100000000]i32 } auto output
-- random input { 100000000i64 1i64 [100000000]i32 } auto output
entry scan_fanin (n: i64) (m: i64) (a: [n * m]i32) =
  scan (+) 0 (map (foldl (+) 0) (unflatten a))

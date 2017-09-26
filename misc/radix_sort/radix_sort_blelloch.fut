-- A least significant digit radix sort to test out `scatter`; this variant
-- directly based on [1], which is apparently also the basis for one of
-- Accelerate's example programs.
--
-- [1] G. E. Blelloch. "Prefix sums and their applications." Technical Report
--     CMU-CS-90-190. Carnegie Mellon University. 1990.
--
-- ==
--
-- input {
--   [83u32, 1u32, 4u32, 99u32, 33u32, 0u32, 6u32, 5u32]
-- }
-- output {
--   [0u32, 1u32, 4u32, 5u32, 6u32, 33u32, 83u32, 99u32]
-- }
--
-- input @ data/radix_sort_100.in
-- output @ data/radix_sort_100.out

import "/futlib/array"

let plus_scan [n] (x: [n]i32): [n]i32 =
  scan (+) 0 x

let plus_prescan [n] (x: [n]i32): [n]i32 =
  let xshifted = map (\(i: i32): i32  -> if i == 0 then 0 else unsafe x[i - 1]) (iota n)
  in scan (+) 0 xshifted

let permute [n] (a: [n]u32, index: [n]i32): [n]u32 =
  scatter (copy a) index a

let plus_scan_reverse_order [n] (x: [n]i32): [n]i32 =
  let xreversed = map (\(i: i32): i32  -> x[n - i - 1]) (iota(n))
  let x' = plus_scan xreversed
  let x'reversed = map (\(i: i32): i32  -> x'[n - i - 1]) (iota(n))
  in x'reversed

let split_blelloch [n] (a: [n]u32, flags: [n]i32): [n]u32 =
  let i_down = plus_prescan(map (1-) flags)
  let i_up = map (n-) (plus_scan_reverse_order(flags))
  let index = map (\(i: i32): i32  ->
                    if flags[i] == 1 then i_up[i] else i_down[i]) (
                  iota(n))
  in permute(a, index)

let split_radix_sort [n] (a: [n]u32, number_of_bits: i32): [n]u32 =
  loop (a) for i < number_of_bits do
    let ai = map (\(a: u32): i32  -> i32((a >> u32(i)) & 1u32)) a
    in split_blelloch(a, ai)

let main [n] (xs: [n]u32): [n]u32 =
  split_radix_sort(xs, 32)

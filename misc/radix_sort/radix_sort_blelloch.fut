-- A least significant digit radix sort to test out `write`; this variant
-- directly based on [1], which is apparently also the basis for one of
-- Accelerate's example programs.
--
-- [1] G. E. Blelloch. "Prefix sums and their applications." Technical Report
--     CMU-CS-90-190. Carnegie Mellon University. 1990.
--
-- ==
--
-- input {
--   [83, 1, 4, 99, 33, 0, 6, 5]
-- }
-- output {
--   [0, 1, 4, 5, 6, 33, 83, 99]
-- }
--
-- input @ data/radix_sort_100.in
-- output @ data/radix_sort_100.out

fun main(xs: [n]u32): [n]u32 =
  split_radix_sort(xs, 32)

fun split_radix_sort(a: [n]u32, number_of_bits: i32): [n]u32 =
  loop (a) = for i < number_of_bits do
    let ai = map(fn (a: u32): i32  => i32((a >> u32(i)) & 1u32), a)
    in split_blelloch(a, ai)
  in a

fun split_blelloch(a: [n]u32, flags: [n]i32): [n]u32 =
  let i_down = plus_prescan(map((1-), flags))
  let i_up = map((n-), plus_scan_reverse_order(flags))
  let index = map(fn (i: i32): i32  =>
                    if flags[i] == 1 then i_up[i] else i_down[i],
                  iota(n))
  in permute(a, index)

fun plus_scan_reverse_order(x: [n]i32): [n]i32 =
  let xreversed = map(fn (i: i32): i32  => x[n - i - 1], iota(n))
  let x' = plus_scan(xreversed)
  let x'reversed = map(fn (i: i32): i32  => x'[n - i - 1], iota(n))
  in x'reversed

fun plus_scan(x: [n]i32): [n]i32 =
  scan((+), 0, x)

fun plus_prescan(x: [n]i32): [n]i32 =
  let xshifted = map(fn (i: i32): i32  => if i == 0 then 0 else unsafe x[i - 1], iota(n))
  in scan((+), 0, xshifted)

fun permute(a: [n]u32, index: [n]i32): [n]u32 =
  write(index, a, copy(a))

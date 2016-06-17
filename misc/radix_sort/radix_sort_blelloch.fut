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

fun [n]u32 main([n]u32 xs) =
  split_radix_sort(xs, 32)

fun [n]u32 split_radix_sort([n]u32 a, i32 number_of_bits) =
  loop (a) = for i < number_of_bits do
    let ai = map(fn i32 (u32 a) => i32((a >> u32(i)) & 1u32), a)
    in split_blelloch(a, ai)
  in a

fun [n]u32 split_blelloch([n]u32 a, [n]i32 flags) =
  let i_down = plus_prescan(map(1 -, flags))
  let i_up = map(n -, plus_scan_reverse_order(flags))
  let index = map(fn i32 (i32 i) =>
                    if flags[i] == 1 then i_up[i] else i_down[i],
                  iota(n))
  in permute(a, index)

fun [n]i32 plus_scan_reverse_order([n]i32 x) =
  let xreversed = map(fn i32 (i32 i) => x[n - i - 1], iota(n))
  let x' = plus_scan(xreversed)
  let x'reversed = map(fn i32 (i32 i) => x'[n - i - 1], iota(n))
  in x'reversed

fun [n]i32 plus_scan([n]i32 x) =
  scan(+, 0, x)

fun [n]i32 plus_prescan([n]i32 x) =
  let xshifted = map(fn i32 (i32 i) => if i == 0 then 0 else unsafe x[i - 1], iota(n))
  in scan(+, 0, xshifted)

fun [n]u32 permute([n]u32 a, [n]i32 index) =
  write(index, a, copy(a))

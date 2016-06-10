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

fun [u32, n] main([u32, n] xs) =
  split_radix_sort(xs, 32)

fun [u32, n] split_radix_sort([u32, n] a, i32 number_of_bits) =
  loop (a) = for i < number_of_bits do
    let ai = map(fn i32 (u32 a) => i32((a >> u32(i)) & 1u32), a)
    in split_blelloch(a, ai)
  in a

fun [u32, n] split_blelloch([u32, n] a, [i32, n] flags) =
  let i_down = plus_prescan(map(1 -, flags))
  let i_up = map(n -, plus_scan_reverse_order(flags))
  let index = map(fn i32 (i32 i) =>
                    if flags[i] == 1 then i_up[i] else i_down[i],
                  iota(n))
  in permute(a, index)

fun [i32, n] plus_scan_reverse_order([i32, n] x) =
  let xreversed = map(fn i32 (i32 i) => x[n - i - 1], iota(n))
  let x' = plus_scan(xreversed)
  let x'reversed = map(fn i32 (i32 i) => x'[n - i - 1], iota(n))
  in x'reversed

fun [i32, n] plus_scan([i32, n] x) =
  scan(+, 0, x)

fun [i32, n] plus_prescan([i32, n] x) =
  let xshifted = map(fn i32 (i32 i) => if i == 0 then 0 else unsafe x[i - 1], iota(n))
  in scan(+, 0, xshifted)

fun [u32, n] permute([u32, n] a, [i32, n] index) =
  write(index, a, copy(a))

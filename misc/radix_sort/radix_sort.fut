-- A least significant digit radix sort to test out `write`.
-- ==
--
-- nobench input {
--   [83, 1, 4, 99, 33, 0, 6, 5]
-- }
-- output {
--   [0, 1, 4, 5, 6, 33, 83, 99]
-- }
--
-- nobench input @ data/radix_sort_100.in
-- output @ data/radix_sort_100.out

fun [n]u32 main([n]u32 xs) =
  radix_sort(xs)

fun [n]u32 radix_sort([n]u32 xs) =
  loop (xs) = for i < 32 do
    radix_sort_step(xs, i)
  in xs

fun [n]u32 radix_sort_step([n]u32 xs, i32 digit_n) =
  let bits = map(fn i32 (u32 x) => i32((x >> u32(digit_n)) & 1u32), xs)
  let bits_inv = map(fn i32 (i32 b) => 1 - b, bits)
  let ps0 = scan(+, 0, bits_inv)
  let ps0_clean = map(*, zip(bits_inv, ps0))
  let ps1 = scan(+, 0, bits)
  let ps0_offset = reduce(+, 0, bits_inv)
  let ps1_clean = map(+ ps0_offset, ps1)
  let ps1_clean' = map(*, zip(bits, ps1_clean))
  let ps = map(+, zip(ps0_clean, ps1_clean'))
  let ps_actual = map(fn i32 (i32 p) => p - 1, ps)
  in write(ps_actual, xs, copy(xs))

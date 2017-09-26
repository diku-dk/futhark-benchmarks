-- A least significant digit radix sort to test out `scatter`.
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

let radix_sort_step [n] (xs: [n]u32, digit_n: i32): [n]u32 =
  let bits = map (\(x: u32): i32  -> i32((x >> u32(digit_n)) & 1u32)) xs
  let bits_inv = map (\(b: i32): i32  -> 1 - b) bits
  let ps0 = scan (+) 0 (bits_inv)
  let ps0_clean = map (*) bits_inv ps0
  let ps1 = scan (+) 0 bits
  let ps0_offset = reduce (+) 0 (bits_inv)
  let ps1_clean = map (+ps0_offset) ps1
  let ps1_clean' = map (*) bits ps1_clean
  let ps = map (+) ps0_clean ps1_clean'
  let ps_actual = map (\(p: i32): i32  -> p - 1) ps
  in scatter (copy xs) ps_actual xs

let radix_sort [n] (xs: [n]u32): [n]u32 =
  loop (xs) for i < 32 do radix_sort_step(xs, i)

let main [n] (xs: [n]u32): [n]u32 =
  radix_sort(xs)

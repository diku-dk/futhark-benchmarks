-- Library functions.

fun *[i32] i32_filter([bool, n] bs, [i32, n] ns) =
  let flags = map(fn bool (i32 n) => bs[n], ns)
  let flags_i = map(fn i32 (bool b) => if b then 1 else 0, flags)
  let is0 = scan(+, 0, flags_i)
  let filter_size = is0[n - 1]
  let is1 = map(fn i32 (i32 i, i32 f) => i * f, zip(is0, flags_i))
  let is2 = map(fn i32 (i32 i) => i - 1, is1)
  in write(is2, ns, replicate(filter_size, 0))

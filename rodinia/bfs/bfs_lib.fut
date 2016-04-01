-- Library functions.

-- FIXME: It *would* be nice to have a proper write-using filter built-in in
-- Futhark instead of an ordinary function (which cannot take an anonymous
-- function).
--
-- This function corresponds to this expression:
--
--   filter(fn bool (i32 n) => bs[n], ns)

fun *[i32] i32_filter([bool, k] bs, [i32, k] ns) =
  let flags = map(fn bool (i32 n) => bs[n], ns)
  let flags_i = map(fn i32 (bool b) => if b then 1 else 0, flags)
  let is0 = scan(+, 0, flags_i)
  let filter_size = is0[k - 1]
  let is1 = map(fn i32 (i32 i, i32 f) => i * f, zip(is0, flags_i))
  let is2 = map(fn i32 (i32 i) => i - 1, is1)
  in write(is2, ns, replicate(filter_size, 0))

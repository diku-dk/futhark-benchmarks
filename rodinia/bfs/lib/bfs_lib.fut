-- Library functions.
-- ==
-- tags { disable }

-- `i32_filter` corresponds to this expression:
--
--   filter(fn bool (i32 n) => bs[n], ns)
--
-- FIXME: It *would* be nice to have a proper write-using filter built-in in
-- Futhark instead of an ordinary function (which cannot take an anonymous
-- function).
fun *[i32] i32_filter([bool, k] bs, [i32, k] ns) =
  let flags = map(fn bool (i32 n) => bs[n], ns)
  let flags_i = map(fn i32 (bool b) => if b then 1 else 0, flags)
  let is0 = scan(+, 0, flags_i)
  let filter_size = is0[k - 1]
  let is1 = map(fn i32 (i32 i, i32 f) => i * f, zip(is0, flags_i))
  let is2 = map(fn i32 (i32 i) => i - 1, is1)
  in write(is2, ns, replicate(filter_size, 0))

-- Convert an inclusive scan into an exclusive scan, although without the last
-- element.
fun [i32, k] i32_excl_scan_from_incl_scan([i32, k] scanned, i32 ne) =
  map(fn i32 (i32 i) => if i == 0 then ne else scanned[i - 1], iota(k))

-- Like scanSegm(+, 0, array, mask).
fun [i32, k] i32_plus_scan_segm([i32, k] array, [bool, k] mask) =
  let {arrayScanned, maskScanned} =
    unzip(scan(fn {i32, bool} ({i32, bool} arg0, {i32, bool} arg1) =>
                 let {a0, m0} = arg0
                 let {a1, m1} = arg1
                 let a' = if m1 then a1 else a0 + a1
                 let m' = m0 || m1
                 in {a', m'},
               {0, False},
               zip(array, mask)))
  in arrayScanned

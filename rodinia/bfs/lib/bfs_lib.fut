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
  let flags = map(fn bool (i32 n) => unsafe bs[n], ns)
  let flags_i = map(fn i32 (bool b) => if b then 1 else 0, flags)
  let is0 = scan(+, 0, flags_i)
  let filter_size = is0[k - 1]
  let is1 = map(fn i32 (i32 i, i32 f) => i * f, zip(is0, flags_i))
  let is2 = map(fn i32 (i32 i) => i - 1, is1)
  in write(is2, ns, replicate(filter_size, 0))

-- Convert an inclusive scan into an exclusive scan, although without the last
-- element.
fun [i32, k] i32_excl_scan_from_incl_scan([i32, k] scanned, i32 ne) =
  map(fn i32 (i32 i) => if i == 0 then ne else unsafe scanned[i - 1], iota(k))

-- Like scanSegm(+, 0, array, mask).
fun [i32, k] i32_plus_scan_segm([i32, k] array, [bool, k] mask) =
  let (arrayScanned, maskScanned) =
    unzip(scan(fn (i32, bool) ((i32, bool) arg0, (i32, bool) arg1) =>
                 let (a0, m0) = arg0
                 let (a1, m1) = arg1
                 let a' = if m1 then a1 else a0 + a1
                 let m' = m0 || m1
                 in (a', m'),
               (0, False),
               zip(array, mask)))
  in arrayScanned

-- Get the updating indices.
fun (*[i32], i32) get_updating_indices([bool] updating_graph_mask) =
  get_updating_indices_alt0(updating_graph_mask)
  
-- Get the updating indices through a filter.
fun (*[i32], i32) get_updating_indices_alt0([bool, n] updating_graph_mask) =
  let updating_indices = i32_filter(updating_graph_mask, iota(n))
  let n_indices = size(0, updating_indices)
  in (updating_indices, n_indices)

-- Alternatively, get the updating indices through two maps and a reduce, and
-- let the non-active indices be -1.  This was found to be slower for both small
-- and large datasets.
fun (*[i32, n], i32) get_updating_indices_alt1([bool, n] updating_graph_mask) =
  let updating_indices = map(fn i32 (i32 i) =>
                               if updating_graph_mask[i] then i else -1,
                             iota(n))
  let zero_ones = map(fn i32 (i32 i) =>
                        if updating_graph_mask[i] then 1 else 0,
                      iota(n))
  let n_indices = reduce(+, 0, zero_ones)
  in (updating_indices, n_indices)

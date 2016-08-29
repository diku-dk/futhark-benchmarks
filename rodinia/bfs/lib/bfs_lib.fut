-- Library functions.
-- ==
-- tags { disable }

-- `i32_filter` corresponds to this expression:
--
--   filter(fn bool (i32 n) => bs[n], ns)
--
-- FIXME: It *would* be nice to have a proper write-using filter built-in
-- Futhark instead of an ordinary function (which cannot take an anonymous
-- function).
fun i32_filter (bs: [k]bool) (ns: [k]i32): *[]i32 =
  let flags = map (fn (n: i32): bool  => unsafe bs[n]) ns
  let flags_i = map (fn (b: bool): i32  => if b then 1 else 0) flags
  let is0 = scan (+) 0 (flags_i)
  let filter_size = is0[k - 1]
  let is1 = map (fn (i: i32, f: i32): i32  => i * f) (zip is0 (flags_i))
  let is2 = map (fn (i: i32): i32  => i - 1) is1
  in write is2 ns (replicate filter_size 0)

-- Convert an inclusive scan into an exclusive scan, although without the last
-- element.
fun i32_excl_scan_from_incl_scan (scanned: [k]i32) (ne: i32): [k]i32 =
  map (fn (i: i32): i32  => if i == 0 then ne else unsafe scanned[i - 1]) (iota(k))

-- Like scanSegm((+), 0, array, mask).
fun i32_plus_scan_segm(array: [k]i32, mask: [k]bool): [k]i32 =
  let (arrayScanned, maskScanned) =
    unzip(scan (fn (arg0: (i32, bool)) (arg1: (i32, bool)): (i32, bool)  =>
                 let (a0, m0) = arg0
                 let (a1, m1) = arg1
                 let a' = if m1 then a1 else a0 + a1
                 let m' = m0 || m1
                 in (a', m')) (0, False) (
               zip array mask))
  in arrayScanned

-- Get the updating indices.
fun get_updating_indices(updating_graph_mask: []bool): (*[]i32, i32) =
  get_updating_indices_alt0(updating_graph_mask)
  
-- Get the updating indices through a filter.
fun get_updating_indices_alt0(updating_graph_mask: [n]bool): (*[]i32, i32) =
  let updating_indices = i32_filter (updating_graph_mask) (iota(n))
  let n_indices = (shape updating_indices)[0]
  in (updating_indices, n_indices)

-- Alternatively, get the updating indices through two maps and a reduce, and
-- let the non-active indices be -1.  This was found to be slower for both small
-- and large datasets.
fun get_updating_indices_alt1(updating_graph_mask: [n]bool): (*[n]i32, i32) =
  let updating_indices = map (fn (i: i32): i32  =>
                               if updating_graph_mask[i] then i else -1) (
                             iota(n))
  let zero_ones = map (fn (i: i32): i32  =>
                        if updating_graph_mask[i] then 1 else 0) (
                      iota(n))
  let n_indices = reduce (+) 0 (zero_ones)
  in (updating_indices, n_indices)

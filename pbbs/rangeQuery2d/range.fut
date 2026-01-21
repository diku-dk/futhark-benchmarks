-- An implementation based on parallel plane sweep.
--
-- Originally written by Shatin Nguyen.
-- ==
-- input @ data/2DinSphere_10K.in
-- output @ data/2DinSphere_10K.out
-- input @ data/2DonSphere_10K.in
-- output @ data/2DonSphere_10K.out
-- input @ data/2Dkuzmin_10K.in
-- output @ data/2Dkuzmin_10K.out
--
-- notest input @ data/2DinSphere_100K.in
-- notest input @ data/2DonSphere_100K.in
-- notest input @ data/2Dkuzmin_100K.in
--
-- notest input @ data/2DinSphere_1M.in
-- notest input @ data/2DonSphere_1M.in
-- notest input @ data/2Dkuzmin_1M.in
--
-- notest input @ data/2DinSphere_10M.in
-- notest input @ data/2DonSphere_10M.in
-- notest input @ data/2Dkuzmin_10M.in
--
-- notest input @ data/2DinSphere_100M.in
-- notest input @ data/2DonSphere_100M.in
-- notest input @ data/2Dkuzmin_100M.in

import "lib/github.com/diku-dk/sorts/radix_sort"
import "lib/github.com/diku-dk/segmented/segmented"
import "util"

def ilog2 (n: i64) : i64 = i64.i32 (63 - i64.clz n)
def lsb (n: i64) : i64 = 1 << i64.i32 (i64.ctz n)

-- Sort points by x-coordinates
def sort_point_by_x [n] (ps: [n]point) : [n]point =
  radix_sort_float_by_key (\p -> p[0]) f64.num_bits f64.get_bit ps

-- Sort an array of f64
def sort [n] (arr: [n]f64) : [n]f64 =
  radix_sort_float f64.num_bits f64.get_bit arr

-- Make flags
def mkFlagArray 't [m]
                (aoa_shp: [m]i64)
                (zero: t)
                (aoa_val: [m]t) : ([m]i64, []t) =
  let shp_rot =
    map (\i ->
           if i == 0
           then 0
           else aoa_shp[i - 1])
        (iota m)
  let shp_scn = scan (+) 0 shp_rot
  let aoa_len =
    if m == 0
    then 0i64
    else shp_scn[m - 1] + aoa_shp[m - 1]
  let shp_ind =
    map2 (\shp ind ->
            if shp == 0
            then -1i64
            else ind)
         aoa_shp
         shp_scn
  let r =
    scatter (replicate aoa_len zero)
            shp_ind
            aoa_val
  in (shp_scn, r)

-- Lower bound binary search
def lower_bound [n] 't (lte: t -> t -> bool) (xs: [n]t) (x: t) : i64 =
  let (l, _) =
    loop (l, r) = (0, n)
    while l < r do
      let t = l + (r - l) / 2
      in if xs[t] `lte` x
         then (t + 1, r)
         else (l, t)
  in l

-- Compress y-coordinates
def compress_y [n] (ys: [n]f64) : ([]f64, [n]i32) =
  let ys_sorted = sort ys
  let ys_idx = map (\y -> i32.i64 (lower_bound (<) ys_sorted y)) ys
  in (ys_sorted, ys_idx)

-- Build prefix structure for rank queries
def build [n] (ys_idx: [n]i32) =
  let S1 = map (\b -> lsb (b + 1)) (iota n)
  let flen = reduce (+) 0 S1
  let (B1, flags') = mkFlagArray S1 0 (iota n)
  let flags = map bool.i64 flags'
  let II1 = segmented_scan (+) 0 flags flags' :> [flen]i64
  let II2' = map2 (\i sgm -> i - B1[sgm]) (iota flen) II1
  let start = map (\sgm -> sgm + 1 - S1[sgm]) II1
  let idx = map2 (+) start II2'
  let II2 = map (\i -> i64.i32 ys_idx[i]) idx
  let ys_incr =
    map (\p ->
           let b = II1[p]
           let y = II2[p]
           in b * n + y)
        (iota flen)
  let ys_sorted = radix_sort_int i64.num_bits i64.get_bit ys_incr
  let ys = map (\p -> i32.i64 (p % n)) ys_sorted
  in (B1, ys)

-- Count how many ranks are < k among first idx points.
def rank [n] (w: ([n]i64, []i32)) (idx: i64) (k: i32) : i32 =
  let (offs, nodes) = w
  let idx = i64.max 0 (i64.min idx n)
  let (acc, _) =
    loop (acc, i) = (0, idx)
    while i > 0 do
      let b = i - 1
      let sz = lsb i
      let off = offs[b]
      let arr = nodes[off:off + sz]
      let t = lower_bound (<) arr k
      in (acc + i32.i64 t, i - lsb i)
  in acc

-- Get point coordinates (x, y) from point array
def points_coord (points: []point) =
  map (\p -> (p[0], p[1])) points

-- Get rectangle coordinates (x1, x2, y1, y2) from rectangle array
def rectangles_coord [m] (rect: [m]rectangle) : [m](f64, f64, f64, f64) =
  map (\rect ->
         let x1 = f64.min rect[0] rect[2]
         let x2 = f64.max rect[0] rect[2]
         let y1 = f64.min rect[1] rect[3]
         let y2 = f64.max rect[1] rect[3]
         in (x1, x2, y1, y2))
      rect

def range [n] [m] (points: [n]point) (rectangles: [m]rectangle) : [m]i32 =
  let points_sorted = sort_point_by_x points
  let (xs, ys) = points_coord points_sorted |> unzip2
  let (ys_values, ys_idx) = compress_y ys
  let w = build ys_idx
  let (x1s, x2s, y1s, y2s) = rectangles_coord rectangles |> unzip4
  let ls = map (\x1 -> lower_bound (<) xs x1) x1s
  let rs = map (\x2 -> lower_bound (<=) xs x2) x2s
  in map (\q ->
            let l = lower_bound (<) ys_values y1s[q] |> i32.i64
            let r = lower_bound (<=) ys_values y2s[q] |> i32.i64
            let right = rank w rs[q] r - rank w rs[q] l
            let left = rank w ls[q] r - rank w ls[q] l
            in right - left)
         (iota m)

entry main (all: []point) : []i32 =
  let (points, rectangles) = split_input all
  in range points rectangles

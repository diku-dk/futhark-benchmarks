-- Based on work by Frederik Berthelsen, Kasper Erik
-- Schmidt-Christensen, Niels Hansen, and Mikkel Kragh Mathiesen.
--
-- Uses single precision floats.
--
-- It is a bit inefficient that we have to sort at the end to get the
-- right ordering, but the flattened quickhull does not otherwise
-- preserve it.
-- ==
-- tags { no_opencl }
-- compiled input @ data/2DinSphere_10K.in
-- output @ data/2DinSphere_10K.out
-- compiled input @ data/2DinSphere_100K.in
-- compiled input @ data/2DinSphere_1M.in
-- compiled input @ data/2DinSphere_10M.in
-- compiled input @ data/2DinSphere_100M.in
-- compiled input @ data/2DonSphere_10K.in
-- compiled input @ data/2DonSphere_100K.in
-- compiled input @ data/2DonSphere_1M.in
-- compiled input @ data/2DonSphere_10M.in
-- compiled input @ data/2DonSphere_100M.in
-- compiled input @ data/2Dkuzmin_10K.in
-- compiled input @ data/2Dkuzmin_100K.in
-- compiled input @ data/2Dkuzmin_1M.in
-- compiled input @ data/2Dkuzmin_10M.in
-- compiled input @ data/2Dkuzmin_100M.in

module type euclidean_space = {
  type dist
  type point

  val zero_dist : dist
  val dist_less : dist -> dist -> bool

  val point_eq : point -> point -> bool
  val point_less : point -> point -> bool

  val signed_dist_to_line : point -> point -> point -> dist
}

module type convex_hull = {
  module space : euclidean_space

  type point = space.point

  val compute [n] : [n]point -> ([]point,[]point)
}

module naive_space : euclidean_space with point = {x:f32, y:f32} = {
  type dist = f32
  type point = {x:f32, y:f32}

  def zero_dist = 0f32
  def dist_less (x : dist) (y : dist) = x < y

  def point_eq (p : point) (q : point) =
    p.x == q.x && p.y == q.y
  def point_less (p : point) (q : point) =
    p.x < q.x || (p.x == q.x && p.y < q.y)

  def sqr (x : f32) = x * x
  def ssqr (x : f32) = f32.abs x * x

  def signed_dist_to_line (p : point) (q : point) (r : point) =
    let ax = q.x - p.x
    let ay = q.y - p.y
    let bx = r.x - p.x
    let by = r.y - p.y
    in ssqr (ax * by - ay * bx) / (sqr ax + sqr ay)
}

module indexed_space (S: euclidean_space)
       : euclidean_space with point = (S.point, i32) = {
  type dist = S.dist
  type point = (S.point, i32)

  def zero_dist = S.zero_dist
  def dist_less = S.dist_less

  def point_eq (a: point) (b: point) = S.point_eq a.0 b.0
  def point_less (a: point) (b: point) = S.point_less a.0 b.0

  def signed_dist_to_line (p: point) (q: point) (r: point) =
    S.signed_dist_to_line p.0 q.0 r.0
}

module quickhull (S : euclidean_space) : convex_hull with space.point = S.point = {
  module space = S
  open space

  def expand_hull [num_segs] [num_points]
                  (segs : [num_segs](point, point))
                  (points : [num_points](i32, point))
    : ([](point, point), [](i32, point)) =
    let dists = map
                (\(seg_ix, p) ->
                   signed_dist_to_line segs[seg_ix].0 segs[seg_ix].1 p)
                points
    let max (i,id) (j,jd) =
      if dist_less jd id then (i,id) else (j,jd)
    let extrema_ix = reduce_by_index
                     (replicate num_segs (-1,zero_dist)) max (-1,zero_dist)
                     (map (i64.i32 <-< (.0)) points) (zip (iota num_points) dists)
    let segs' = tabulate num_segs
                         (\i -> [(segs[i].0, points[extrema_ix[i].0].1),
                                 (points[extrema_ix[i].0].1, segs[i].1)])
                |> flatten
    let eval_point (ix, (seg_ix, p)) =
      if extrema_ix[seg_ix].0 == ix then (-1, p) else
      let (a, b) = segs[seg_ix]
      let q = points[extrema_ix[seg_ix].0].1
      let daq = signed_dist_to_line a q p
      let dqb = signed_dist_to_line q b p
      in if dist_less zero_dist daq then (seg_ix * 2, p)
         else if dist_less zero_dist dqb then (seg_ix * 2 + 1, p)
         else (-1, p)
    let points' =
      filter ((>= 0) <-< (.0)) (eval_point (zip (iota num_points) points))
    in (segs', points')

  def extract_empty_segments [num_segs] [num_points]
                             (hull : [](point))
                             (segs : [num_segs](point, point))
                             (points : [num_points](i32, point))
      : ([](point), [](point, point), [](i32, point)) =
    let point_ixs = map (i64.i32 <-< (.0)) points
    let segs_inhabited =
      reduce_by_index
      (replicate num_segs 0i32) (+) 0 point_ixs (replicate num_points 1) > 0
    let (segs_true, segs_false) = partition (.1) (zip segs segs_inhabited)
    let segs_indicator = i32.bool segs_inhabited
    let new_segs_ix = scan (+) 0 segs_indicator - segs_indicator
    let hull' = hull ++ map (.0.0) segs_false
    let segs' = map (.0) segs_true
    let points' = map (\(seg_ix, p) -> (new_segs_ix[seg_ix], p)) points
    in (hull', segs', points')

  def semihull (start : point) (end : point) (points : []point) =
    if null points then [start]
    else
      let (hull', _, _) = (loop (hull, segs, points) =
         ([], [(start, end)], zip 0 points)
       while !(null points) do
       let (segs', points') = expand_hull segs points
       in extract_empty_segments hull segs' points')
      in hull'

  def pmin p q = if point_less p q then p else q
  def pmax p q = if point_less p q then q else p

  def compute (ps : []point) =
    if length ps <= 3 then (ps, []) else
    let leftmost = reduce pmin ps[0] ps
    let rightmost = reduce pmax ps[0] ps
    let (_, upper_points, lower_points) =
      partition2
      (\p -> point_eq p leftmost || point_eq p rightmost)
      (\p -> dist_less zero_dist (signed_dist_to_line leftmost rightmost p))
      ps
    let upper_hull = semihull leftmost rightmost upper_points
    let lower_hull = semihull rightmost leftmost lower_points
    in (upper_hull, lower_hull)
}

module space = (indexed_space naive_space)
module naive_quickhull = quickhull space

type point = space.point

import "lib/github.com/diku-dk/sorts/radix_sort"
def sort_by f = radix_sort_float_by_key f f32.num_bits f32.get_bit

def clockwise (convex_upper: []point) (convex_lower: []point) =
  let sorted_upper = convex_upper |> sort_by (.0.y) |> sort_by (.0.x)
  let sorted_lower = convex_lower |> sort_by (.0.y) |> sort_by (.0.x)

  let upper_is = map (.1) sorted_upper
  let lower_is = map (.1) (reverse sorted_lower)
  in upper_is++lower_is

def main [k] (ps : [k][2]f64) : []i32 =
  let ps' = map2 (\i p -> ({x=f32.f64 p[0], y=f32.f64 p[1]}, i32.i64 i))
                 (indices ps) ps
  let (convex_upper, convex_lower) = naive_quickhull.compute ps'
  in clockwise convex_upper convex_lower

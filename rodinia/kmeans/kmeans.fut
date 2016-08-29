-- multi-dimensional spatial Euclid distance square
--
-- Much inspiration has been gained from the kmeans-vector package on
-- Hackage.
--
-- ==
-- tags { futhark-opencl futhark-c }
-- nobench input @ data/trivial.in
-- output @ data/trivial.out
-- compiled input @ data/100.in
-- output @ data/100.out
-- notravis input @ data/204800.in
-- output @ data/204800.out
-- notravis input @ data/kdd_cup.in
-- output @ data/kdd_cup.out

fun euclid_dist_2(pt1: [numdims]f32) (pt2: [numdims]f32): f32 =
  reduce (+) (0.0f32) (map (**2.0f32) (zipWith (-) pt1 pt2))

fun closest_point(p1: (int,f32)) (p2: (int,f32)): (int,f32) =
  let (_,d1) = p1 in
  let (_,d2) = p2 in
  if d1 < d2 then p1 else p2

fun find_nearest_point(pts: [k][d]f32) (pt: [d]f32): int =
  let (i, _) = reduceComm closest_point (0, euclid_dist_2 pt pts[0]) (
                          zip (iota(k)) (
                              map (euclid_dist_2(pt)) pts)) in
  i

fun add_centroids(x: [d]f32, y: [d]f32): *[d]f32 =
  zipWith (+) x y

fun centroids_of(k: int, points: [n][d]f32, membership: [n]int): *[k][d]f32 =
  let points_in_clusters =
     streamRedPer (fn (acc: [k]int) (x: [k]int): [k]int  =>
                    zipWith (+) acc x) (
                  fn (chunk: int)
                     (acc: *[ncluster]int)
                     (inp: []int): [k]int  =>
                    loop (acc) = for i < chunk do
                      let c = inp[i] in
                      unsafe let acc[c] = acc[c] + 1 in
                      acc in
                    acc) (
                  replicate k 0) membership in
  let cluster_sums =
    streamRedPer (fn (acc: [k][d]f32)
                    (elem: [k][d]f32): [k][d]f32  =>
                   zipWith (fn (x: []f32, y: []f32): [d]f32  =>
                             add_centroids(x, y)) acc elem) (
                 fn (chunk: int)
                    (acc: *[k][d]f32)
                    (inp: []([d]f32,int)): [k][d]f32  =>
                   loop (acc) = for i < chunk do
                     let (point, c) = inp[i] in
                     unsafe let acc[c] = add_centroids(acc[c], map (/(f32(points_in_clusters[c]))) point) in
                     acc in
                   acc) (
                 replicate k (replicate d 0.0f32)) (
                 zip points membership) in
  cluster_sums

fun main(threshold: int,
       k: int,
       max_iterations: int,
       points: [n][d]f32): ([][]f32, int) =
  -- Assign arbitrary initial cluster centres.
  let cluster_centres = map (fn (i: int): [d]f32  =>
                              unsafe points[i]) (
                            iota(k)) in
  -- Also assign points arbitrarily to clusters.
  let membership = map (%k) (iota(n)) in
  let delta = threshold + 1 in
  let i = 0 in
  loop ((membership, cluster_centres, delta, i)) = while delta > threshold && i < max_iterations do
    -- For each point, find the cluster with the closest centroid.
    let new_membership = map (find_nearest_point cluster_centres) points in
    -- Then, find the new centres of the clusters.
    let new_centres = centroids_of(k, points, new_membership) in
    let delta = reduce (+) 0 (map (fn (b: bool): int  =>
                                   if b then 0 else 1) (
                                 zipWith (==) membership (new_membership))) in
    (new_membership, new_centres, delta, i+1) in
  (cluster_centres, i)

-- multi-dimensional spatial Euclid distance square
--
-- Much inspiration has been gained from the kmeans-vector package on
-- Hackage.
--
-- ==
-- tags { futhark-opencl futhark-c }
-- compiled input @ data/trivial.in
-- output @ data/trivial.out
-- compiled input @ data/100.in
-- output @ data/100.out
-- notravis input @ data/kdd_cup.in
-- output @ data/kdd_cup.out

fun f32 euclid_dist_2([numdims]f32 pt1, [numdims]f32 pt2) =
  reduce(+, 0.0f32, map(**2.0f32, zipWith(-, pt1, pt2)))

fun (int,f32) closest_point((int,f32) p1, (int,f32) p2) =
  let (_,d1) = p1 in
  let (_,d2) = p2 in
  if d1 < d2 then p1 else p2

fun int find_nearest_point([k][d]f32 pts, [d]f32 pt) =
  let (i, _) = reduceComm(closest_point,
                          (0, euclid_dist_2(pt,pts[0])),
                          zip(iota(k),
                              map(euclid_dist_2(pt), pts))) in
  i

fun *[d]f32 add_centroids([d]f32 x, [d]f32 y) =
  zipWith(+, x, y)

fun *[k][d]f32
  centroids_of(int k, [n][d]f32 points, [n]int membership) =
  let points_in_clusters =
     streamRedPer(fn [k]int ([k]int acc, [k]int x) =>
                    zipWith(+, acc, x),
                  fn [k]int (int chunk,
                               *[ncluster]int acc,
                               []int inp) =>
                    loop (acc) = for i < chunk do
                      let c = inp[i] in
                      unsafe let acc[c] = acc[c] + 1 in
                      acc in
                    acc,
                  replicate(k,0), membership) in
  let cluster_sums =
    streamRedPer(fn [k][d]f32 ([k][d]f32 acc,
                                 [k][d]f32 elem) =>
                   zipWith(fn [d]f32 ([]f32 x, []f32 y) =>
                             add_centroids(x, y),
                           acc, elem),
                 fn [k][d]f32 (int chunk,
                                 *[k][d]f32 acc,
                                 []([d]f32,int) inp) =>
                   loop (acc) = for i < chunk do
                     let (point, c) = inp[i] in
                     unsafe let acc[c] = add_centroids(acc[c], map(/f32(points_in_clusters[c]), point)) in
                     acc in
                   acc,
                 replicate(k,replicate(d,0.0f32)),
                 zip(points, membership)) in
  cluster_sums

fun ([][]f32, int)
  main(int threshold,
       int k,
       int max_iterations,
       [n][d]f32 points) =
  -- Assign arbitrary initial cluster centres.
  let cluster_centres = map(fn [d]f32 (int i) =>
                              unsafe points[i],
                            iota(k)) in
  -- Also assign points arbitrarily to clusters.
  let membership = map(% k, iota(n)) in
  let delta = threshold + 1 in
  let i = 0 in
  loop ((membership, cluster_centres, delta, i)) = while delta > threshold && i < max_iterations do
    -- For each point, find the cluster with the closest centroid.
    let new_membership = map(find_nearest_point(cluster_centres), points) in
    -- Then, find the new centres of the clusters.
    let new_centres = centroids_of(k, points, new_membership) in
    let delta = reduce(+, 0, map(fn int (bool b) =>
                                   if b then 0 else 1,
                                 zipWith(==, membership, new_membership))) in
    (new_membership, new_centres, delta, i+1) in
  (cluster_centres, i)

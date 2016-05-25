-- This is kmeans mangled to support only two dimensions, and to use
-- roughly the same algorithm as the kmeans implementation in
-- accelerate-examples.

fun f32 euclid_dist_2((f32,f32) c1, (f32,f32) c2) =
  let (x1,y1) = c1 in
  let (x2,y2) = c2 in
  (x2-x1)**2.0f32 + (y2-y1)**2.0f32

fun (int,f32) closest_point((int,f32) p1, (int,f32) p2) =
  let (_,d1) = p1 in
  let (_,d2) = p2 in
  if d1 < d2 then p1 else p2

fun int find_nearest_point([(f32,f32),k] pts, (f32,f32) pt) =
  let (i, _) = reduceComm(closest_point,
                          (0, euclid_dist_2(pt,pts[0])),
                          zip(iota(k),
                              map(euclid_dist_2(pt), pts))) in
  i

fun (f32,f32) add_centroids((f32,f32) c1, (f32,f32) c2) =
  let (x1,y1) = c1 in
  let (x2,y2) = c2 in
  (x1+x2, y1+y2)

fun *[(f32,f32),k]
  centroids_of(int k, [(f32,f32),n] points, [int,n] membership) =
  let (cluster_counts, cluster_points) =
    unzip(map(fn [(int, (f32,f32)),n] (int cluster) =>
                zipWith(fn (int, (f32,f32)) (int point_cluster, (f32,f32) point) =>
                          if cluster == point_cluster
                          then (1, point)
                          else (0, (0.0f32, 0.0f32)),
                        membership, points),
                iota(k))) in
  let cluster_sizes = map(fn int ([int,n] counts) =>
                            reduce(+, 0, counts),
                          cluster_counts) in
  let cluster_centres = zipWith(fn (f32,f32) (int count, [(f32,f32),n] my_points) =>
                                  let (x,y) =
                                    reduceComm(add_centroids, (0f32, 0f32), my_points) in
                                  (x / f32(count), y / f32(count)),
                                cluster_sizes, cluster_points) in
  cluster_centres

fun f32 fabs32(f32 x) =
  if x < 0.0f32 then -x else x

fun ([(f32,f32)], int)
  main(int threshold,
       int k,
       int max_iterations,
       [[f32,d],n] points) =
  let points = map(fn (f32,f32) ([f32,d] point) =>
                     (point[0], point[1]),
                   points) in

  -- Assign arbitrary initial cluster centres.
  let cluster_centres = map(fn (f32,f32) (int i) =>
                              unsafe points[i],
                            iota(k)) in
  -- Also assign points arbitrarily to clusters.
  let membership = map(% k, iota(n)) in
  let continue = True in
  let i = 0 in
  loop ((membership, cluster_centres, continue, i)) = while continue && i < max_iterations do
    -- For each point, find the cluster with the closest centroid.
    let new_membership = map(find_nearest_point(cluster_centres), points) in
    -- Then, find the new centres of the clusters.
    let new_centres = centroids_of(k, points, new_membership) in
    let continue = reduce(||, False,
                          zipWith(fn bool ((f32,f32) c1, (f32,f32) c2) =>
                                    let (x1,y1) = c1 in
                                    let (x2,y2) = c2 in
                                    fabs32(x1-x2) > 0.01f32 || fabs32(y1-y2) > 0.01f32,
                                  copy(new_centres), cluster_centres)) in
    (new_membership, new_centres, continue, i+1) in
  (cluster_centres, i)

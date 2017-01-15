-- This is kmeans mangled to support only two dimensions, and to use
-- roughly the same algorithm as the kmeans implementation
-- accelerate-examples.

fun euclid_dist_2(c1: (f32,f32)) (c2: (f32,f32)): f32 =
  let (x1,y1) = c1
  let (x2,y2) = c2
  in (x2-x1)**2.0f32 + (y2-y1)**2.0f32

fun closest_point(p1: (i32,f32)) (p2: (i32,f32)): (i32,f32) =
  let (_,d1) = p1
  let (_,d2) = p2
  in if d1 < d2 then p1 else p2

fun find_nearest_point(pts: [k](f32,f32)) (pt: (f32,f32)): i32 =
  let (i, _) = reduceComm closest_point (0, euclid_dist_2 pt pts[0]) (
                          zip (iota(k)) (
                              map (euclid_dist_2(pt)) pts))
  in i

fun add_centroids(c1: (f32,f32)) (c2: (f32,f32)): (f32,f32) =
  let (x1,y1) = c1
  let (x2,y2) = c2
  in (x1+x2, y1+y2)

fun centroids_of(k: i32, points: [n](f32,f32), membership: [n]i32): *[k](f32,f32) =
  let (cluster_counts, cluster_points) =
    unzip(map (\(cluster: i32): [n](i32,(f32,f32))  ->
                map (\(point_cluster: i32) (point: (f32,f32)): (i32, (f32,f32))  ->
                          if cluster == point_cluster
                          then (1, point)
                          else (0, (0.0f32, 0.0f32))) membership points) (
                iota(k)))
  let cluster_sizes = map (\(counts: [n]i32): i32  ->
                            reduce (+) 0 counts) (
                          cluster_counts)
  let cluster_centres = map (\(count: i32) (my_points: [n](f32,f32)): (f32,f32)  ->
                                  let (x,y) =
                                    reduceComm add_centroids (0f32, 0f32) my_points
                                  in (x / f32(count), y / f32(count)))
                                  cluster_sizes cluster_points
  in cluster_centres

fun fabs32(x: f32): f32 =
  if x < 0.0f32 then -x else x

fun main(threshold: i32,
       k: i32,
       max_iterations: i32,
       points: [n][d]f32): ([]f32,[]f32, i32) =
  let points = map (\(point: [d]f32): (f32,f32)  ->
                     (point[0], point[1])) points

  -- Assign arbitrary initial cluster centres.
  let cluster_centres = map (\(i: i32): (f32,f32)  ->
                              unsafe points[i]) (
                            iota(k))
  -- Also assign points arbitrarily to clusters.
  let membership = map (%k) (iota(n))
  let continue = true
  let i = 0
  loop ((membership, cluster_centres, continue, i)) = while continue && i < max_iterations do
    -- For each point, find the cluster with the closest centroid.
    let new_membership = map (find_nearest_point(cluster_centres)) points
    -- Then, find the new centres of the clusters.
    let new_centres = centroids_of(k, points, new_membership)
    let continue = reduce (||) false (
                          map (\(c1: (f32,f32)) (c2: (f32,f32)): bool  ->
                                    let (x1,y1) = c1
                                    let (x2,y2) = c2
                                    in fabs32(x1-x2) > 0.01f32 || fabs32(y1-y2) > 0.01f32) (
                                  copy(new_centres)) (cluster_centres))
    in (new_membership, new_centres, continue, i+1)
  in (#0 (unzip(cluster_centres)), #1 (unzip(cluster_centres)), i)

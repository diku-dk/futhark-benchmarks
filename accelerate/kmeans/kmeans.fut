-- This is an implementation of K-means specialised for only two
-- dimensions.  See [0] for an implementation that works for arbitrary
-- dimensionality - it's not all that more complicated.
--
-- [0]: https://github.com/diku-dk/futhark-benchmarks/tree/master/rodinia/kmeans
-- ==
-- compiled input @ data/trivial.in
-- output @ data/trivial.out
-- compiled input @ data/k5_n50000.in
-- output @ data/k5_n50000.out
-- compiled input @ data/k5_n200000.in
-- output @ data/k5_n200000.out

type point = (f32,f32)

let add_points((x1,y1): point) ((x2,y2): point): point =
  (x1+x2, y1+y2)

let euclid_dist_2((x1,y1): point) ((x2,y2): point): f32 =
  (x2-x1)**2.0f32 + (y2-y1)**2.0f32

let closest_point(p1: (i32, f32)) (p2: (i32, f32)): (i32, f32) =
  let (_,d1) = p1
  let (_,d2) = p2
  in if d1 < d2 then p1 else p2

let find_nearest_point [k] (pts: [k]point) (pt: point): i32 =
  let (i, _) = reduce_comm closest_point (0, euclid_dist_2 pt pts[0])
                           (zip [0..<k] (map (euclid_dist_2 pt) pts))
  in i

let centroids_of [n] (k: i32) (points: [n]point) (membership: [n]i32): [k]point =
  let (cluster_counts, cluster_points) =
    unzip (map (\cluster  ->
                 map (\point_cluster point ->
                        if cluster == point_cluster
                        then (1, point)
                        else (0, (0.0f32, 0.0f32)))
                     membership points)
              [0..<k])
  let cluster_sizes =
    map (\counts -> reduce (+) 0 (intrinsics.opaque counts))
        cluster_counts
  let cluster_centres =
    map (\count my_points  ->
           let (x,y) = reduce_comm add_points (0f32, 0f32) my_points
           in (x / f32 count, y / f32 count))
        (intrinsics.opaque cluster_sizes) cluster_points
  in cluster_centres

let continue [k] (old_centres: [k]point) (cur_centres: [k]point): bool =
  reduce (||) false (map (\(x1,y1) (x2,y2) ->
                          f32.abs (x1-x2) > 0.01f32 || f32.abs(y1-y2) > 0.01f32)
                     old_centres (intrinsics.opaque cur_centres))

let main [n] (k: i32,
              points_in: [n][2]f32): ([][2]f32, i32) =
  -- Transform from 2D-array to array of pairs.
  let points = map (\point -> (point[0], point[1])) points_in

  -- Assign arbitrary initial cluster centres.
  let initial_cluster_centres = take k points

  let recentre (cluster_centres: [k]point) =
        -- For each point, find the cluster with the closest centroid.
        let new_membership = map (find_nearest_point cluster_centres) points
        -- Then find the new centres of the clusters.
        in centroids_of k points new_membership

  let (_, final_cluster_centres, i) =
    loop (old_centres, cur_centres, i) =
      (initial_cluster_centres, recentre initial_cluster_centres, 0)
    while continue old_centres cur_centres do
      (cur_centres, recentre cur_centres, i+1)
  in (map (\(x, y) -> [x,y]) final_cluster_centres, i)

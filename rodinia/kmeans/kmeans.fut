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
-- input @ data/204800.in.gz
-- output @ data/204800.out
-- input @ data/kdd_cup.in.gz
-- output @ data/kdd_cup.out

let euclid_dist_2 [d] (pt1: [d]f32) (pt2: [d]f32): f32 =
  f32.sum (map (**2.0f32) (map2 (-) pt1 pt2))

let closest_point (p1: (i32,f32)) (p2: (i32,f32)): (i32,f32) =
  if p1.2 < p2.2 then p1 else p2

let find_nearest_point [k][d] (pts: [k][d]f32) (pt: [d]f32): i32 =
  let (i, _) = reduce_comm closest_point (0, euclid_dist_2 pt pts[0])
               (zip (iota k) (map (euclid_dist_2 pt) pts))
  in i

let add_centroids [d] (x: [d]f32) (y: [d]f32): *[d]f32 =
  map2 (+) x y

let centroids_of [n][d] (k: i32, points: [n][d]f32, membership: [n]i32): [k][d]f32 =
  let points_in_clusters =
     stream_red_per (\(acc: [k]i32) (x: [k]i32) ->
                     map2 (+) acc x)
                    (\(inp: []i32) ->
                     loop acc = (replicate k 0) for c in inp do
                       unsafe let acc[c] = acc[c] + 1 in acc)
                    membership
  let cluster_sums =
    stream_red_per (\(acc: [k][d]f32) (elem: [k][d]f32) ->
                    map2 add_centroids acc elem)
                   (\(inp: []([d]f32,i32)) ->
                       loop acc = replicate k (replicate d 0f32) for (point,c) in inp do
                         unsafe let acc[c] =
                                  add_centroids acc[c] (map (/(r32(points_in_clusters[c]))) point)
                                in acc)
                   (zip points membership)
  in cluster_sums

let main [n][d]
        (threshold: i32) (k: i32) (max_iterations: i32)
        (points: [n][d]f32): ([][]f32, i32) =
  -- Assign arbitrary initial cluster centres.
  let cluster_centres = map (\(i: i32): [d]f32  ->
                               unsafe points[i])
                            (iota k)
  -- Also assign points arbitrarily to clusters.
  let membership = map (%k) (iota n)
  let delta = threshold + 1
  let i = 0
  let (_,cluster_centres,_,i) =
    loop (membership, cluster_centres, delta, i)
    while delta > threshold && i < max_iterations do
      -- For each point, find the cluster with the closest centroid.
      let new_membership = map (find_nearest_point cluster_centres) points
      -- Then, find the new centres of the clusters.
      let new_centres = centroids_of(k, points, new_membership)
      let delta = i32.sum (map (\b -> if b then 0 else 1)
                               (map2 (==) membership new_membership))
      in (new_membership, new_centres, delta, i+1)
  in (cluster_centres, i)

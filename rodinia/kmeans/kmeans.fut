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
-- input @ data/204800.in
-- output @ data/204800.out
-- input @ data/kdd_cup.in
-- output @ data/kdd_cup.out

let euclid_dist_2 (pt1: [#numdims]f32) (pt2: [#numdims]f32): f32 =
  reduce (+) 0.0f32 (map (**2.0f32) (map (-) pt1 pt2))

let closest_point (p1: (i32,f32)) (p2: (i32,f32)): (i32,f32) =
  if #2 p1 < #2 p2 then p1 else p2

let find_nearest_point(pts: [#k][#d]f32) (pt: [#d]f32): i32 =
  let (i, _) = reduce_comm closest_point (0, euclid_dist_2 pt pts[0])
               (zip (iota k) (map (euclid_dist_2 pt) pts))
  in i

let add_centroids(x: [#d]f32) (y: [#d]f32): *[d]f32 =
  map (+) x y

let centroids_of(k: i32, points: [#n][#d]f32, membership: [#n]i32): *[k][d]f32 =
  let points_in_clusters =
     stream_red_per (\(acc: [#k]i32) (x: [#k]i32) ->
                     map (+) acc x)
                  (\(inp: [#chunk]i32) ->
                     stream_seq (\(acc: *[#k]i32) (inp': [#chunk']i32) ->
                                loop (acc) for i < chunk' do
                                  let c = inp'[i]
                                  in unsafe let acc[c] = acc[c] + 1
                                            in acc)
                              (replicate k 0) inp)
                  membership
  let cluster_sums =
    stream_red_per (\(acc: [#k][#d]f32) (elem: [#k][#d]f32) ->
                    map add_centroids acc elem)
                 (\ (inp: [#chunk]([#d]f32,i32)) ->
                   stream_seq (\(acc: *[#k][#d]f32) (inp': [#chunk']([#d]f32,i32)) ->
                     loop (acc) for i < chunk' do
                       let (point, c) = inp'[i]
                       in unsafe let acc[c] =
                                   add_centroids acc[c] (map (/(f32(points_in_clusters[c]))) point)
                                 in acc) (replicate k (replicate d 0f32)) inp)
                 (zip points membership)
  in cluster_sums

let main(threshold: i32, k: i32, max_iterations: i32,
         points: [#n][#d]f32): ([][]f32, i32) =
  -- Assign arbitrary initial cluster centres.
  let cluster_centres = map (\(i: i32): [d]f32  ->
                               unsafe points[i])
                            (iota k)
  -- Also assign points arbitrarily to clusters.
  let membership = map (%k) (iota n)
  let delta = threshold + 1
  let i = 0
  let (_,cluster_centres,_,i) =
    loop ((membership, cluster_centres, delta, i))
    while delta > threshold && i < max_iterations do
      -- For each point, find the cluster with the closest centroid.
      let new_membership = map (find_nearest_point cluster_centres) points
      -- Then, find the new centres of the clusters.
      let new_centres = centroids_of(k, points, new_membership)
      let delta = reduce (+) 0 (map (\(b: bool): i32  ->
                                       if b then 0 else 1)
                                (map (==) membership new_membership))
      in (new_membership, new_centres, delta, i+1)
  in (cluster_centres, i)

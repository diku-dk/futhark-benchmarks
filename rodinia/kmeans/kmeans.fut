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

def euclid_dist_2 [d] (pt1: [d]f32) (pt2: [d]f32): f32 =
  f32.sum ((pt1 - pt2)**2)

def closest_point (p1: (i32,f32)) (p2: (i32,f32)): (i32,f32) =
  if p1.1 < p2.1 then p1 else p2

def find_nearest_point [k][d] (pts: [k][d]f32) (pt: [d]f32): i32 =
  let (i, _) = foldl (\acc (i, p) -> closest_point acc (i32.i64 i, euclid_dist_2 pt p))
                     (0, f32.inf)
                     (zip (indices pts) pts)
  in i

def centroids_of [n][d] (k: i64) (points: [n][d]f32) (membership: [n]i32): [k][d]f32 =
  let points_in_clusters =
    hist (+) 0 k (i64.i32 membership) (replicate n 1)

  let cluster_sums =
    hist (map2 (+)) (replicate d 0) k
         (i64.i32 membership)
         points

  let prop c = replicate d (f32.i64 (if c == 0 then 1 else c))

  in cluster_sums / prop points_in_clusters

def main [n][d]
        (threshold: i32) (k: i32) (max_iterations: i32)
        (points: [n][d]f32): ([][]f32, i32) =
  let k = i64.i32 k

  -- Assign arbitrary initial cluster centres.
  let cluster_centres = take k points
  -- Also assign points arbitrarily to clusters.
  let membership = i32.i64 (iota n % k)
  let delta = threshold + 1
  let i = 0
  let (_,cluster_centres,_,i) =
    loop (membership, cluster_centres, delta, i)
    while delta > threshold && i < max_iterations do
      -- For each point, find the cluster with the closest centroid.
      let new_membership = find_nearest_point cluster_centres points
      -- Then, find the new centres of the clusters.
      let new_centres = centroids_of k points new_membership
      let delta = i32.sum (i32.bool (membership != new_membership))
      in (new_membership, new_centres, delta, i+1)
  in (cluster_centres, i)

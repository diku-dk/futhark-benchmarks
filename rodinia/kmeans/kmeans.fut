-- multi-dimensional spatial Euclid distance square
--
-- Much inspiration has been gained from the kmeans-vector package on
-- Hackage.
--
-- ==
--
-- compiled input @ data/trivial.in
-- output @ data/trivial.out
-- compiled input @ data/100.in
-- output @ data/100.out
-- notravis input @ data/kdd_cup.in
-- output @ data/kdd_cup.out

fun f32 euclid_dist_2([f32,numdims] pt1, [f32,numdims] pt2) =
  reduce(+, 0.0f32, map(**2.0f32, zipWith(-, pt1, pt2)))

fun {int,f32} closest_point({int,f32} p1, {int,f32} p2) =
  let {_,d1} = p1 in
  let {_,d2} = p2 in
  if d1 < d2 then p1 else p2

fun int find_nearest_point([[f32,nfeatures],npoints] pts, [f32,nfeatures] pt) =
  let {i, _} = reduce(closest_point,
                      {0, euclid_dist_2(pt,pts[0])},
                      zip(iota(npoints),
                          map(euclid_dist_2(pt), pts))) in
  i

fun *[f32,nfeatures] add_centroids([f32,nfeatures] x, [f32,nfeatures] y) =
  zipWith(+, x, y)

fun *[[f32,nfeatures],nclusters]
  centroids_of(int nclusters, [[f32,nfeatures],npoints] feature, [int,npoints] membership) =
  let features_in_clusters =
     streamRedPer(fn *[int,nclusters] ([int,nclusters] acc,
                                       [int,nclusters] x) =>
                    zipWith(+, acc, x),
                  fn [int,nclusters] (int chunk,
                                      *[int,ncluster] acc,
                                      [int] inp) =>
                    loop (acc) = for i < chunk do
                      let c = inp[i] in
                      unsafe let acc[c] = acc[c] + 1 in
                      acc in
                    acc,
                  replicate(nclusters,0), membership) in
  let cluster_sums =
    streamRedPer(fn *[[f32,nfeatures],nclusters] (*[[f32,nfeatures],nclusters] acc,
                                                  *[[f32,nfeatures],nclusters] elem) =>
                   zipWith(fn [f32,nfeatures] ([f32] x, [f32] y) =>
                             add_centroids(x, y),
                           acc, elem),
                 fn *[[f32,nfeatures],nclusters] (int chunk,
                                                   *[[f32,nfeatures],nclusters] acc,
                                                   [{[f32,nfeatures], int}] inp) =>
                   loop (acc) = for i < chunk do
                     let {point, c} = inp[i] in
                     unsafe let acc[c] = add_centroids(acc[c], map(/f32(features_in_clusters[c]), point)) in
                     acc in
                   acc,
                 replicate(nclusters,replicate(nfeatures,0.0f32)),
                 zip(feature, membership)) in
  cluster_sums

fun {[[f32]], [int], int}
  main(int threshold,
       int nclusters,
       int max_iterations,
       [[f32,nfeatures],npoints] feature) =
  -- Assign arbitrary initial cluster centres.
  let cluster_centres = map(fn [f32,nfeatures] (int i) =>
                              unsafe feature[i],
                            iota(nclusters)) in
  -- Also assign points arbitrarily to clusters.
  let membership = map(% nclusters, iota(npoints)) in
  let delta = threshold + 1 in
  let i = 0 in
  loop ({membership, cluster_centres, delta, i}) = while delta > threshold && i < max_iterations do
    -- For each point, find the cluster with the closest centroid.
    let new_membership = map(find_nearest_point(cluster_centres), feature) in
    -- Then, find the new centres of the clusters.
    let new_centres = centroids_of(nclusters, feature, new_membership) in
    let delta = reduce(+, 0, map(fn int (bool b) =>
                                   if b then 0 else 1,
                                 zipWith(==, membership, new_membership))) in
    {new_membership, new_centres, delta, i+1} in
  {cluster_centres, membership, i}

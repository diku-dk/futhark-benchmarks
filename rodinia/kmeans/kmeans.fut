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

fun real euclid_dist_2([real,numdims] pt1, [real,numdims] pt2) =
  reduce(+, 0.0, map(**2.0, zipWith(-, pt1, pt2)))

fun {int,real} closest_point({int,real} p1, {int,real} p2) =
  let {_,d1} = p1 in
  let {_,d2} = p2 in
  if d1 < d2 then p1 else p2

fun int find_nearest_point([[real,nfeatures],npoints] pts, [real,nfeatures] pt) =
  let {i, _} = reduce(closest_point,
                      {0, euclid_dist_2(pt,pts[0])},
                      zip(iota(npoints),
                          map(euclid_dist_2(pt), pts))) in
  i

fun *[real,nfeatures] add_centroids([real,nfeatures] x, [real,nfeatures] y) =
  zipWith(+, x, y)

fun *[[real,nfeatures],nclusters]
  centroids_of(int nclusters, [[real,nfeatures],npoints] feature, [int,npoints] membership) =
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
    streamRedPer(fn *[[real,nfeatures],nclusters] (*[[real,nfeatures],nclusters] acc,
                                                   *[[real,nfeatures],nclusters] elem) =>
                   zipWith(fn [real,nfeatures] ([real] x, [real] y) =>
                             add_centroids(x, y),
                           acc, elem),
                 fn *[[real,nfeatures],nclusters] (int chunk,
                                                   *[[real,nfeatures],nclusters] acc,
                                                   [{[real,nfeatures], int}] inp) =>
                   loop (acc) = for i < chunk do
                     let {point, c} = inp[i] in
                     unsafe let acc[c] = add_centroids(acc[c], map(/real(features_in_clusters[c]), point)) in
                     acc in
                   acc,
                 replicate(nclusters,replicate(nfeatures,0.0)),
                 zip(feature, membership)) in
  cluster_sums

fun {[[real]], [int], int}
  main(int threshold,
       int nclusters,
       int max_iterations,
       [[real,nfeatures],npoints] feature) =
  -- Assign arbitrary initial cluster centres.
  let cluster_centres = map(fn [real,nfeatures] (int i) =>
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

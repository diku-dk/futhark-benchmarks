Straightforward implementation of k-means clustering.  Not all of the
Rodinia data sets are present here, because they are fairly large.

There are some changes compared to Rodinia:

  0) The threshold value is integral instead of floating-point.  This
     reflects how it is actually used, which is to indicate the lowest
     number of cluster reassignments we permit in an iteration before
     we finish the convergence loops.

  1) There is a maximum number of iterations (this is also present in
     some Rodinia implementations).

Both of the above values are set on a per-dataset basis.

The implementation outputs three values:

  0) The cluster centres.

  1) A membership vector mapping points to cluster numbers.

  2) The number of iterations done by the convergence loop.

Straightforward port of the OpenMP implementation.  Uses explicit
indexing.  The number of iterations to run is also given in the input
data sets - these are currently all 360.

This is a fairly trivial two-dimensional rank-1 stencil, although with
some unusual edge conditions.  Performance in Futhark is OK.

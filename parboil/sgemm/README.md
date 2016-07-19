According to the documentation, SGEMM is merely a matrix multiply.
However, all the benchmarks actually perform the computation

    C * beta + alpha * (A*B)

Where A, B and C are matrices.  This is congruent with SGEMM as known
from
[LAPACK](http://www.math.utah.edu/software/lapack/lapack-blas/sgemm.html).

The Parboil implementations assume specific layouts of the matrices
(column-major etc).  Our Futhark implementation does not.

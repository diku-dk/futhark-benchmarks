-- | Implementations of [QR
-- decomposition](https://en.wikipedia.org/wiki/QR_decomposition).  At
-- the moment, these are not very fast for single large matrices, but
-- they are serviceable.  Performance is quite good on "batches" on
-- many smaller matrices (i.e. when you `map` QR decomposition), where
-- "small" is less than 16x16 or 32x16.
--
-- Much of this code is based on work by Kasper Unn Weihe, Kristian
-- Quirin Hansen, and Peter Kanstrup Larsen.  See [their
-- report](https://futhark-lang.org/student-projects/kristian-kasper-peter-project.pdf)
-- for details.

import "linalg"

-- The implementations below are somewhat numerically unstable, and in
-- particular don't properly have _actual_ zeroes below the diagonal
-- (but they'll be close!).  So, we force them to be.
local
def zero_below_main_diag [n][m] 't (zero: t) (R: [m][n]t): [m][n]t =
  map2 (\i -> map2 (\j x -> if j < i then zero else x) (iota n))
       (iota m) R

-- | QR decomposition via the blocked Householder transform.  The
-- block size affects performance, although usually only slightly.
-- Use 16 for a reasonable default.  **At the moment, the input size
-- must be a multiple of the block size.**
module mk_block_householder (T: ordered_field) : {
  val qr [m][n] : (block_size: i64) -> (A: [m][n]T.t) -> ([m][m]T.t, [m][n]T.t)
} = {
  def dotprod [n] (xs: [n]T.t) (ys: [n]T.t): T.t =
    reduce (T.+) (T.i64 0) (map2 (T.*) xs ys)

  def matvecmul_row [n][m] (xss: [n][m]T.t) (ys: [m]T.t) =
    map (\xs -> dotprod ys xs) xss

  def identity (n: i64): [n][n]T.t =
    tabulate_2d n n (\i j -> if j == i then T.i64 1 else T.i64 0)

  def sqrt x = T.(x ** (i64 1 / i64 2))

  def house [d] (x: [d]T.t): (*T.t, *T.t) =
    let dot = dotprod x x
    let v0 = T.(x[0] - sqrt dot)
    let dot' = T.(dot - x[0]*x[0] + v0*v0)
    let beta = T.(if dot' != i64 0 then i64 2/dot' else i64 0)
    in (copy v0, copy beta)

  def matmul [n][p][m] (xss: [n][p]T.t) (yss: [p][m]T.t): *[n][m]T.t =
    map (\xs -> map (\ys -> #[sequential] dotprod xs ys) (transpose yss)) xss

  def outer [n][m] (xs: [n]T.t) (ys: [m]T.t): *[n][m]T.t =
    map (\x -> map (\y -> x T.* y) ys) xs

  def matsub [m][n] (xss: [m][n]T.t) (yss: [m][n]T.t): *[m][n]T.t =
    map2 (\xs ys -> map2 (T.-) xs ys) xss yss

  def matadd [m][n] (xss: [m][n]T.t) (yss: [m][n]T.t): *[m][n]T.t =
    map2 (map2 (T.+)) xss yss

  def matmul_scalar [m][n] (xss: [m][n]T.t) (k: T.t): *[m][n]T.t =
    map (map (T.* k)) xss

  def vecmul_scalar [m] (xs: [m]T.t) (k: T.t): *[m]T.t =
    map (T.* k) xs

  def qr [m][n] (r: i64) (A: [m][n]T.t): ([m][m]T.t, [m][n]T.t) =
    let (Q,R) =
      loop (Q,A) = copy (identity m, A) for k in 0..<(n/r) do
      let s = k * r
      let V = replicate m (replicate r (T.i64 0))
      let Bs = replicate r (T.i64 0)

      let (Bs, V, A) =
        loop (Bs, V, A) for j in 0..<r do
        let u = s + j
        let (v0, B) = house A[u:,u]
        let V[u+1:,j] = A[u+1:,u]
        let V[k * r + j:u + 1, j] = [v0]
        let l1 = m-u
        let l2 = r-j
        let v = V[k * r + j:, j] :> [l1]T.t
        let a_slice = A[u:, u:s+r] :> *[l1][l2]T.t
        let BvvT = matmul_scalar (outer v v) B
        let BvvTAk = matmul BvvT a_slice
        let A[u:, u:s+r] = matsub a_slice BvvTAk
        let Bs[j] = B
        in (Bs, V, A)

      let Y = replicate r (replicate m (T.i64 0))
      let W = replicate r (replicate m (T.i64 0))
      let Y[0] = V[:, 0]
      let W[0] = vecmul_scalar Y[0] (T.neg Bs[0])

      let (Y, W) =
        loop (Y, W) for j in 1..<r do
        let v = V[:, j]
        let WYTv = matvecmul_row (matmul (transpose W[0:j]) Y[0:j]) v
        let BWYTv = vecmul_scalar WYTv Bs[j]
        let mbj = vecmul_scalar v (T.neg Bs[j])
        let z = map2 (T.-) mbj BWYTv
        let Y[j] = v
        let W[j] = z
        in (Y, W)

      let l = m - s
      let YWTA = matmul (matmul (transpose Y)  W) A[:, s+r:n]
      let A[:, s+r:n] = matadd A[:, s+r:n] YWTA
      let WY = (matmul (transpose W[:, s:]) Y[:, s:]) :> [l][l]T.t
      let Q_block = (Q[:, s:]) :> *[m][l]T.t
      let QWY = matmul Q_block WY
      let Q[:, s:] = matadd Q_block QWY
      in (Q,A)
    in (Q, zero_below_main_diag (T.i64 0) R)
}

-- | QR decomposition with the Gram-Schmidt process.  Note: Very
-- numerically unstable.
module mk_gram_schmidt (T: ordered_field) : {
  val qr [m][n] : (A: [m][n]T.t) -> ([m][m]T.t, [m][n]T.t)
} = {
  def dotprod [n] (xs: [n]T.t) (ys: [n]T.t): T.t =
    reduce (T.+) (T.i64 0) (map2 (T.*) xs ys)

  def matmul [n][p][m] (xss: [n][p]T.t) (yss: [p][m]T.t): *[n][m]T.t =
    map (\xs -> map (dotprod xs) (transpose yss)) xss

  def outer [n][m] (xs: [n]T.t) (ys: [m]T.t): *[n][m]T.t =
    map (\x -> map (\y -> x T.* y) ys) xs

  def vecdiv_scalar [m] (xs: [m]T.t) (k: T.t): *[m]T.t =
    map (T./ k) xs

  def vecmin [n] (xs: [n]T.t) (ys: [n]T.t): *[n]T.t =
    map2 (T.-) xs ys

  def matvecmul_col [m][n] (xss: [n][m]T.t) (ys: [m]T.t) =
    map (\xs -> map2 (T.*) xs ys) xss

  def sqrt x = T.(x ** (i64 1 / i64 2))

  def vector_length [m] (xs: [m]T.t): T.t =
    let vector_sum = reduce (T.+) (T.i64 0) (map (\x -> (x T.* x)) xs)
    in sqrt vector_sum

  def sum_row [m][n] (xss: [m][n]T.t) =
      map (\xs -> reduce (T.+) (T.i64 0) xs) xss

  def qr [m][n] (A: [m][n]T.t): (*[m][m]T.t, *[m][n]T.t) =
    let Q = replicate m (replicate m (T.i64 0))
    let Q[:,0] = vecdiv_scalar A[:,0] (vector_length A[:,0])
    let Q =
      loop Q for i in 1..<(n) do
      let q = Q[:,:i]
      let sum_qA = sum_row (matvecmul_col (transpose q) A[:, i])
      let sum_qAq = sum_row (matvecmul_col q sum_qA)
      let q' = vecmin A[:,i] sum_qAq
      let Q[:,i] = q'
      let Q[:,i] = vecdiv_scalar Q[:,i] (vector_length Q[:,i])
      in Q
    let R = matmul (transpose Q) A
    in (Q, zero_below_main_diag (T.i64 0) R)
}

-- | Operations for performing LU-decomposition with partial (row) pivoting and
-- operations for solving systems of linear equations using the LU-decomposition
-- functionality. The module `mk_lup` is parameterised over an ordered
-- field. Examples of ordered fields include `f64` and `f32`.

import "linalg"
import "perm"

local module type lup = {

  -- | Type of elements
  type t

  -- | Perform LU-decomposition with partial (row) pivoting. A call `lup A`
  -- returns a pair `(LU,P)` of a matrix `LU` and a permutation `P`, such that
  -- `permute P A = matmul (lower LU) (upper LU)`. Notice that both the lower
  -- and upper triangular matrices are embedded in the resulting matrix `LU`;
  -- see the functions `lower` and `upper` below.
  val lup [n] : (mat:*[n][n]t) -> ([n][n]t, perm.t[n])

  -- | Forward solving. The expression `forsolve L b` solves (`Lx = b`, `x`),
  -- where `x` and `b` are vectors and `L` is a lower-triangular matrix. Reads
  -- only lower part of `L`, excluding the diagonal, and assumes implicit unit
  -- diagonal elements.
  val forsolve [n] : [n][n]t -> [n]t -> [n]t

  -- | Backward solving. The expression `backsolve U y` solves (`Ux = y`, `x`),
  -- where `x` and `y` are vectors and `U` is an upper-triangular square matrix.
  -- Reads only upper part of `U`, including the diagonal.
  val backsolve [n] : [n][n]t -> [n]t -> [n]t

  -- | Solve a linear system using LU-decomposition with partial (row) pivoting.
  val ols [n] : *[n][n]t -> *[n]t -> *[n]t

  -- | Perform LU-decomposition without pivoting. A call `lu A` returns a matrix
  -- `LU` containing the lower and upper parts of the decomposition. Due to the
  -- lack of pivoting, this function is unstable. Perhaps use the `lup` function
  -- instead. On success, if `lu A = LU` then `A = matmul (lower LU) (upper
  -- LU)`.
  val lu [n] : (mat:*[n][n]t) -> [n][n]t

  -- | Return the unit-lower matrix embedded in the argument matrix. Extracts
  -- all lower, non-diagonel elements and returns a matrix containing the unit
  -- element in diagonal entries and the zero element in strict-upper entries.
  val lower [n] : [n][n]t -> [n][n]t

  -- | Return the upper matrix embedded in the argument matrix. Extracts all
  -- upper elements, including the diagonal elements. The resulting matrix has
  -- the zero element in all strict-lower entries.
  val upper [n] : [n][n]t -> [n][n]t

}

-- | LU-decomposition module parameterised over an ordered field.
module mk_lup (T: ordered_field) : lup with t = T.t
= {

  type t = T.t

  def z = T.i64 0

  def dotprod [n] (a: [n]T.t) (b: [n]T.t): T.t =
    map2 (T.*) a b |> reduce (T.+) (T.i64 0)

  def step [m] (mat:*[m][m]t) (p:perm.t[m]) (j:i64) : (*[m][m]t, perm.t[m]) =
    let jp = reduce (\(a:(i64,t)) (b:(i64,t)) : (i64,t) -> if a.1 T.> b.1 then a else b) (-1,z)
		    (map2 (\i v -> (i+j,T.abs v)) (iota (m-j)) (mat[j:,j])) |> (.0)
    let pv = copy(mat[jp][j])
    let mat = perm.permute (perm.swap (perm.id m) jp j) mat
    let p = perm.swap p jp j
    let mat[j+1:,j] = map (T./ pv) mat[j+1:,j]
    let mat[j+1:,j+1:] =
      tabulate_2d (m-j-1) (m-j-1)
		  (\r c ->
		     mat[r+j+1][c+j+1] T.- (mat[r+j+1][j] T.* mat[j][c+j+1])
		  )
    in (mat,p)

  -- Perform LU-decomposition with partial (row) pivoting. A call `lup a`
  -- returns a pair `(LU,p)` of a matrix `LU` and a permutation `p`, such that
  -- `permute p a = LU`.
  def lup [m] (mat:*[m][m]t) : ([m][m]t, perm.t[m]) =
    loop (mat, p) = (mat, perm.id m) for j < m do step mat p j

  -- Solve (Lx = b, x), where x and b are vectors and L is a lower-triangular
  -- matrix. Reads only lower part of L, excluding the diagonal, and assumes
  -- implicit unit diagonal elements.
  def forsolve [n] (L:[n][n]t) (b:[n]t) : [n]t =
    let y : *[n]t = replicate n (T.i64 0)
    in loop y for i in 0..<n do
       let sum = dotprod L[i,:i] y[:i]
       let y[i] = copy(b[i] T.- sum)
       in y

  -- Solve (Ux = y, x), where x and y are vectors and U is an upper-triangular
  -- square matrix.  Reads only upper part of U, including the diagonal.
  def backsolve [n] (U:[n][n]t) (y:[n]t) : [n]t =
    let x : *[n]t = replicate n (T.i64 0)
    in loop x for j in 0..<n do
       let i = n - j - 1
       let sum = dotprod U[i,i+1:] x[i+1:]
       let x[i] = copy(y[i] T.- sum) T./ U[i,i]
       in x

  def ols [n] (a: *[n][n]t) (b:*[n]t) : *[n]t =
    let (LU,p) = lup a
    in backsolve LU (forsolve LU (perm.permute p b))

  def lower [n] (a:[n][n]t) =
    tabulate_2d n n (\i j -> if i==j then T.i64 1 else
			     if i > j then a[i][j] else T.i64 0)

  def upper [n] (a:[n][n]t) =
    tabulate_2d n n (\i j -> if i <= j then a[i][j] else T.i64 0)

  -- Perform LU-decomposition without pivoting. A call `lu a` returns a matrix
  -- `LU` containing the lower and upper parts of the decomposition.
  def lu [m] (mat:*[m][m]t) : [m][m]t =
    loop mat = mat for j < m
    do let pv = copy(mat[j][j])
       let mat[j+1:,j] = map (T./ pv) mat[j+1:,j]
       let mat[j+1:,j+1:] =
	 tabulate_2d (m-j-1) (m-j-1)
		     (\r c ->
			mat[r+j+1][c+j+1] T.- (mat[r+j+1][j] T.* mat[j][c+j+1])
		     )
       in mat

}

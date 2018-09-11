-- | Small library of simple linear algebra-ish operations.

local module type linalg = {
  type t
  -- | Dot product.
  val dotprod [n]: [n]t -> [n]t -> t
  -- | Outer product.
  val outer [n] [m]: [n]t -> [m]t -> [n][m]t
  -- | Cross product (only for three-element vectors).
  val cross: [3]t -> [3]t -> [3]t
  -- | Multiply a matrix with a row vector.
  val matvecmul_row [n][m]: [n][m]t -> [m]t -> [n]t
  -- | Multiply a matrix with a column vector.
  val matvecmul_col [n][m]: [n][m]t -> [n]t -> [n][n]t
  -- | Multiply two matrices.
  val matmul [n][p][m]: [n][p]t -> [p][m]t -> [n][m]t
  -- | Kronecker product of two matrices.
  val kronecker [n][p][m]: [n][p]t -> [p][m]t -> [][]t
  -- | Kronecker product of two matrices, but preserving the blocked
  -- structure in the result.
  val kronecker' [m][n][p][q]: [m][n]t -> [p][q]t -> [m][n][p][q]t
  -- | Compute the inverse of a matrix.
  val inv [n]: [n][n]t -> [n][n]t
  -- | Solve linear system.
  val ols [n][m]: [n][m]t -> [n]t -> [m]t
}

-- | Given some numeric type, produce a linalg module.
module mk_linalg (T: numeric): linalg with t = T.t = {

  type t = T.t

  let dotprod [n] (xs: [n]t) (ys: [n]t): t =
    T.(reduce (+) (i32 0) (map2 (*) xs ys))

  let cross (xs: [3]t) (ys: [3]t): [3]t =
    T.([xs[1]*ys[2]-xs[2]*ys[1],
        xs[2]*ys[0]-xs[0]*ys[2],
        xs[0]*ys[1]-xs[1]*ys[0]])

  let matmul [n][p][m] (xss: [n][p]t) (yss: [p][m]t): [n][m]t =
    map (\xs -> map (dotprod xs) (transpose yss)) xss

  let outer [n][m] (xs: [n]t) (ys: [m]t): [n][m]t =
    matmul (map (\x -> [x]) xs) [ys]

  let matvecmul_row [n][m] (xss: [n][m]t) (ys: [m]t) =
    map (dotprod ys) xss

  let matvecmul_col [n][m] (xss: [n][m]t) (ys: [n]t) =
    matmul xss (replicate m ys)

  let kronecker' [m][n][p][q] (xss: [m][n]t) (yss: [p][q]t): [m][n][p][q]t =
    map (map (\x -> map (map (T.*x)) yss)) xss

  let kronecker [m][n][p][q] (xss: [m][n]t) (yss: [p][q]t): [][]t =
    kronecker' xss yss -- [m][n][p][q]
    |> map transpose   -- [m][p][n][q]
    |> flatten         -- [m*p][n][q]
    |> map flatten     -- [m*p][n*q]

  -- Matrix inversion is implemented with Gauss-Jordan.
  let gauss_jordan [n][m] (A: [n][m]t): [n][m]t =
    loop A for i < n do
      let irow = A[0]
      let Ap = A[1:n]
      let v1 = irow[i]
      in  map (\k -> map (\j -> let x = unsafe( T.(irow[j] / v1) ) in
                                if k < n-1  -- Ap case
                                then unsafe( T.(Ap[k,j] - Ap[k,i] * x) )
                                else x      -- irow case
                         ) (iota m)
              ) (iota n)

  let inv [n] (A: [n][n]t): [n][n]t =
    -- Pad the matrix with the identity matrix.
    let Ap = map2 (\row i ->
                    map (\j -> if j < n then unsafe( row[j] )
                                     else if j == n+i
                                          then T.i32 1
                                          else T.i32 0
                        ) (iota (2*n))
                  ) A (iota n)
    let Ap' = gauss_jordan Ap
    -- Drop the identity matrix at the front.
    in Ap'[0:n, n:n*2]

  let ols [n][m] (X: [n][m]t) (b: [n]t): [m]t =
    matvecmul_row (matmul (inv (matmul (transpose X) X)) (transpose X)) b
}

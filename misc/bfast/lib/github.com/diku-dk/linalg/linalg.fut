-- | Small library of simple linear algebra-ish operations.

-- | The result of applying `mk_linalg`@term.  Note that this module
-- type is declared `local`, which means you cannot directly reference
-- it by name from outside.  This is because it is not a stable
-- interface, as we may add new members in minor versions.
local module type linalg = {
  -- | The scalar type.
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
  val kronecker [n][m][p][q]: [m][n]t -> [p][q]t -> [][]t
  -- | Kronecker product of two matrices, but preserving the blocked
  -- structure in the result.
  val kronecker' [m][n][p][q]: [m][n]t -> [p][q]t -> [m][n][p][q]t
  -- | Compute the inverse of a matrix.
  val inv [n]: [n][n]t -> [n][n]t
  -- | Solve linear system.
  val ols [n][m]: [n][m]t -> [n]t -> [m]t
}

-- An algebraic
-- [field](https://en.wikipedia.org/wiki/Field_(mathematics)), with
-- some added things. The `mk_linalg` module takes one of these as
-- arguments.  The builtin modules `f32`@term and `f64`@term satisfy
-- this interface.
module type field = {
  type t

  val +: t -> t -> t
  val -: t -> t -> t
  val *: t -> t -> t
  val /: t -> t -> t
  val **: t -> t -> t

  val neg: t -> t
  val <: t -> t -> bool

  val i64: i64 -> t
  val abs: t -> t
  val fma: t -> t -> t -> t
}

-- A field extended with an ordering relation.
module type ordered_field = {
  include field

  val ==: t -> t -> bool
  val <: t -> t -> bool
  val >: t -> t -> bool
  val <=: t -> t -> bool
  val >=: t -> t -> bool
  val !=: t -> t -> bool
}

-- | Given some numeric type, produce a linalg module.
module mk_linalg (T: field): linalg with t = T.t = {

  type t = T.t

  def dotprod [n] (xs: [n]t) (ys: [n]t): t =
    T.(reduce (+) (i64 0) (map2 (*) xs ys))

  def cross (xs: [3]t) (ys: [3]t): [3]t =
    T.([xs[1]*ys[2]-xs[2]*ys[1],
        xs[2]*ys[0]-xs[0]*ys[2],
        xs[0]*ys[1]-xs[1]*ys[0]])

  def matmul [n][p][m] (xss: [n][p]t) (yss: [p][m]t): [n][m]t =
    map (\xs -> map (dotprod xs) (transpose yss)) xss

  def outer [n][m] (xs: [n]t) (ys: [m]t): [n][m]t =
    matmul (map (\x -> [x]) xs) [ys]

  def matvecmul_row [n][m] (xss: [n][m]t) (ys: [m]t) =
    map (dotprod ys) xss

  def matvecmul_col [n][m] (xss: [n][m]t) (ys: [n]t) =
    matmul xss (replicate m ys)

  def kronecker' [m][n][p][q] (xss: [m][n]t) (yss: [p][q]t): [m][n][p][q]t =
    map (map (\x -> map (map (T.*x)) yss)) xss

  def kronecker [m][n][p][q] (xss: [m][n]t) (yss: [p][q]t): [][]t =
    kronecker' xss yss        -- [m][n][p][q]
    |> map transpose          -- [m][p][n][q]
    |> flatten                -- [m*p][n][q]
    |> map (flatten_to (n*q)) -- [m*p][n*q]

  def indices_from [n] 't (x: i64) (arr: [n]t) =
    zip arr (map (+x) (iota n))

  def argmax arr =
    reduce_comm (\(a,i) (b,j) ->
                   if a T.< b
                   then (b,j)
                   else if b T.< a then (a,i)
                   else if j < i then (b, j)
                   else (a, i))
                (T.i64 0, 0)
                (zip arr (indices arr))

  -- Matrix inversion is implemented with Gauss-Jordan.
  def gauss_jordan [m] [n] (A:[m][n]t) =
    loop A for i < i64.min m n do
    -- Find nonzero value.
    let j = A[i:,i] |> map T.abs |> argmax |> (.1) |> (+i)
    let f = T.((i64 1-A[i,i]) / A[j,i])
    let irow = map2 (T.fma f) A[j] A[i]
    in tabulate m (\j ->
                     let f = T.neg A[j,i]
                     in map2 (\x y -> if j == i then x else T.fma f x y)
                             irow A[j])

  def inv [n] (A: [n][n]t): [n][n]t =
    -- Pad the matrix with the identity matrix.
    let twon = 2*n
    let Ap = map2 (\row i ->
                    map (\j -> if j < n then row[j]
                                     else if j == n+i
                                          then T.i64 1
                                          else T.i64 0
                        ) (iota twon)
                  ) A (iota n)
    let Ap' = gauss_jordan Ap
    -- Drop the identity matrix at the front.
    in Ap'[0:n, n:n*2] :> [n][n]t

  def ols [n][m] (X: [n][m]t) (b: [n]t): [m]t =
    matvecmul_row (matmul (inv (matmul (transpose X) X)) (transpose X)) b
}

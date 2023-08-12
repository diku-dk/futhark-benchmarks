-- | Small library of simple linear algebra-ish operations.

-- | The result of applying `mk_linalg`@term.  Note that this module
-- type is declared `local`, which means you cannot directly reference
-- it by name from outside.  This is because it is not a stable
-- interface, as we may add new members in minor versions.
local module type linalg = {
  -- | The scalar type.
  type t

  ------ CREATING MATRICES/VECTORS ------

  -- | Make a zero vector.
  val veczeros : (n : i64) -> [n]t
  -- | Make a ones vector.
  val vecones : (n : i64) -> [n]t
  -- | Create an array of n values varying linearly from the first to second argument.
  val linspace : t -> t -> (n:i64) -> [n]t
  -- | Make an identity matrix.
  val eye : (n : i64) -> [n][n]t
  -- | Make a zero matrix.
  val matzeros : (n : i64) -> (m : i64) -> [n][m]t
  -- | Make a ones matrix.
  val matones : (n : i64) -> (m : i64) -> [n][m]t
  -- | Cast scalar values up into matrices (simulated broadcasting).
  val mat [m][n] : t -> [m][n]t
  -- | Cast integers up into matrices (simulated broadcasting).
  val i64 [m][n] : i64 -> [m][n]t
  -- | Cast floating point numbers up into matrices (simulated broadcasting).
  val f64 [m][n] : f64 -> [m][n]t

  ------ CORE FUNCTIONS ------

  -- | Dot product.
  val dotprod [n]: [n]t -> [n]t -> t
  -- | Outer product.
  val outer [n] [m]: [n]t -> [m]t -> *[n][m]t
  -- | Cross product (only for three-element vectors).
  val cross: [3]t -> [3]t -> [3]t
  -- | Multiply a matrix with a row vector.
  val matvecmul_row [n][m]: [n][m]t -> [m]t -> *[n]t
  -- | Multiply a matrix with a column vector.
  val matvecmul_col [n][m]: [n][m]t -> [n]t -> *[n][n]t
  -- | Multiply two matrices.
  val matmul [n][p][m]: [n][p]t -> [p][m]t -> *[n][m]t

  -- | Form a block matrix from 4 submatrices.
  val block [m1][m2][n1][n2] : (A: [m1][n1]t) -> (B: [m1][n2]t) -> (C: [m2][n1]t) -> (D: [m2][n2]t) -> [m1+m2][n1+n2]t
  -- | A general way to apply a unary operator (e.g., (neg), etc.) element-wise to a matrix.
  val matunary [n][m] : (t->t) -> [n][m]t -> [n][m]t
  -- | A general way to apply an operator (e.g., (+), (-), etc.) element-wise between two matrices.
  val matop [n][m] : (t->t->t) -> [n][m]t -> [n][m]t -> [n][m]t
  -- | A general way to apply a comparison operator (e.g., (==), (<), etc.) element-wise between two matrices.
  val matcomp [n][m] : (t->t->bool) -> [n][m]t -> [n][m]t -> [n][m]bool
  -- | Scale a vector by some constant.
  val vecscale [n] : t -> [n]t -> *[n]t
  -- | Scale a matrix by some constant.
  val matscale [n][m] : t -> [n][m]t -> *[n][m]t
  -- | Compute the difference of two matrices.
  val matsub [m][n] : [m][n]t -> [m][n]t -> *[m][n]t
  -- | Compute the sum of two matrices.
  val matadd [m][n] : [m][n]t -> [m][n]t -> *[m][n]t
  -- | Compute the L2 norm of a vector.
  val vecnorm [n] : [n]t -> t
  -- | Return a matrix containing only the diagonal of the argument.
  val matdiag [n] : [n][n]t -> [n][n]t
  -- | Return a vector of the input matrix's diagonal.
  val fromdiag [n] : [n][n]t -> [n]t
  -- | Return a diagonal matrix with its diagonal being the input vector.
  val todiag [n] : [n]t -> [n][n]t

  -- | Kronecker product of two matrices.
  val kronecker [n][m][p][q]: [m][n]t -> [p][q]t -> *[m*p][n*q]t
  -- | Kronecker product of two matrices, but preserving the blocked
  -- structure in the result.
  val kronecker' [m][n][p][q]: [m][n]t -> [p][q]t -> *[m][n][p][q]t
  -- | Compute the inverse of a matrix.
  val inv [n]: [n][n]t -> *[n][n]t
  -- | Solve a linear system.
  val ols [n][m]: [n][m]t -> [n]t -> *[m]t

  -- | Compute the hessenberg form H of input matrix and its transformation matrix Q. Returns (H, Q).
  val househess [n] : [n][n]t -> ([n][n]t, [n][n]t)
  -- | Compute the eigenvalue decomposition of matrix.
  -- Returns (D, V), where D is a diagonal matrix of the eigenvalues, and the columns of V are their eigenvectors.
  val eig [n] : [n][n]t -> ([n][n]t, [n][n]t)
  -- | Compute the square root of the input matrix.
  val matsqrt [n] : [n][n]t -> [n][n]t

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

  val i64: i64 -> t
  val f64: f64 -> t
  val abs: t -> t
  val fma: t -> t -> t -> t
  val sqrt: t -> t
  val isnan: t -> bool
  val isinf: t -> bool
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
module mk_linalg (T: ordered_field): linalg with t = T.t = {

  type t = T.t

  
  def veczeros (N) : [N]t =
    T.(map (\_ -> i64 0) (0..<N))

  def vecones (N) : [N]t =
    map (\_ -> T.i64 1) (0..<N)

  def linspace (start: t)(stop: t)(n: i64) : [n]t =
    let step = T.((stop - start) / (i64 n - i64 1))
    in tabulate n (\i -> T.(i64 i * step + start))

  def eye (n : i64) : [n][n]t =
    tabulate_2d n n (\i j -> if i == j then T.i64 1 else T.i64 0)

  def matzeros (N) (M): [N][M]t =
  map (\_ -> map (\_->T.i64 0) (0..<M)) (0..<N)

  def matones (N) (M): [N][M]t =
    map (\_ -> map (\_->T.i64 1) (0..<M)) (0..<N)


  def dotprod [n] (xs: [n]t) (ys: [n]t): t =
    T.(reduce (+) (i64 0) (map2 (*) xs ys))

  def cross (xs: [3]t) (ys: [3]t): [3]t =
    T.([xs[1]*ys[2]-xs[2]*ys[1],
        xs[2]*ys[0]-xs[0]*ys[2],
        xs[0]*ys[1]-xs[1]*ys[0]])

  def matmul [n][p][m] (xss: [n][p]t) (yss: [p][m]t): *[n][m]t =
    map (\xs -> map (dotprod xs) (transpose yss)) xss

  def outer [n][m] (xs: [n]t) (ys: [m]t): *[n][m]t =
    matmul (map (\x -> [x]) xs) [ys]

  def matvecmul_row [n][m] (xss: [n][m]t) (ys: [m]t) =
    map (dotprod ys) xss

  def matvecmul_col [n][m] (xss: [n][m]t) (ys: [n]t) =
    matmul xss (replicate m ys)

  def block [m1][m2][n1][n2] (A: [m1][n1]t) (B: [m1][n2]t) (C: [m2][n1]t) (D: [m2][n2]t) : [m1+m2][n1+n2]t =
    transpose (transpose (A ++ C) ++ transpose (B ++ D))

  --a general way to apply an operator (e.g., (+), (-), etc.) element-wise between two tensors
  def matunary [n][m] (op: t->t) (A: [n][m]t) : [n][m]t = 
    map (\as -> map op as) A

  def matop [n][m] (op: t->t->t) (A: [n][m]t) (B: [n][m]t): [n][m]t = 
    map2 (\as bs -> map2 op as bs) A B

  def matcomp [n][m] (op: t->t->bool) (A: [n][m]t) (B: [n][m]t): [n][m]bool = 
    map2 (\as bs -> map2 op as bs) A B

  def vecscale [n] (scalar : t) (arr : [n]t) : *[n]t =
    map (T.* scalar) arr

  def matscale [n][m] (scalar: t) (A : [n][m]t): *[n][m]t =
    map (map (T.* scalar)) A

  def matsub [m][n] (xss: [m][n]t) (yss: [m][n]t): *[m][n]t =
    map2 (\xs ys -> map2 (T.-) xs ys) xss yss

  def matadd [m][n] (xss: [m][n]t) (yss: [m][n]t): *[m][n]t =
    map2 (map2 (T.+)) xss yss

  def vecnorm [N] (x: [N]t) : t =
    T.sqrt (reduce (T.+) (T.i64 0) (map (T.**(T.i64 2)) x))

  def matdiag [n] (A : [n][n]t): [n][n]t =
    tabulate_2d n n (\i j -> if i == j then A[i,j] else T.i64 0)

  def fromdiag [n] (A : [n][n]t): [n]t =
    tabulate n (\i -> A[i,i])

  def todiag [n] (x : [n]t): [n][n]t =
    tabulate_2d n n (\i j -> if i == j then x[i] else T.i64 0)

  def kronecker' [m][n][p][q] (xss: [m][n]t) (yss: [p][q]t): *[m][n][p][q]t =
    map (map (\x -> map (map (T.*x)) yss)) xss

  def kronecker [m][n][p][q] (xss: [m][n]t) (yss: [p][q]t): *[m*p][n*q]t =
    kronecker' xss yss        -- [m][n][p][q]
    |> map transpose          -- [m][p][n][q]
    |> flatten                -- [m*p][n][q]
    |> map flatten            -- [m*p][n*q]

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

  def inv [n] (A: [n][n]t): *[n][n]t =
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
    in Ap'[0:n, n:n*2] :> *[n][n]t

  def ols [n][m] (X: [n][m]t) (b: [n]t): *[m]t =
    matvecmul_row (matmul (inv (matmul (transpose X) X)) (transpose X)) b

    
  def vhouse [n] (x: [n]t): ([n]t, t) =
    let normx = T.i64 1 T./ vecnorm x
    let x = if T.isnan normx || T.isinf normx then x else vecscale normx x --OK?
    let s = dotprod x[1:] x[1:]
    let v = [T.i64 1] ++ x[1:] :> [n]t
    in if T.(s == i64 0)
      then (v, T.i64 0)
      else
        let mu = T.(sqrt (x[0] ** (i64 2) + s))
        let v0 = T.(if x[0] <= i64 0 then x[0]-mu else (neg s)/(x[0]-mu))
        let beta = T.((i64 2) * (v0 ** (i64 2)) / (s + v0 ** (i64 2)))
        let v' = vecscale T.(i64 1 / v0) ([v0] ++ x[1:])
        in if T.isnan v0 || T.isnan beta then (v, T.i64 0) else (v' :> [n]t, beta)

  --should be OK. Verified against examples computed with scipy
  def househess [n] (A : [n][n]t): ([n][n]t, [n][n]t) =
    loop (H, Q) = (copy A,eye n) for k in 0..<(n-2) do
      let m = n-(k+1)
      let a = n-k
      let (v, beta) = vhouse (H[k+1:,k]) :> ([m]t, t)
      let I = eye (k+1)
      let N = matzeros (k+1) m
      let R = matop (T.-) (eye m) (matscale beta (outer v v))
      let H = H with [k+1:,k:] = matmul R (H[k+1:,k:] :> [m][a]t)
      let H = H with [:,k+1:] = matmul (H[:,k+1:] :> [n][m]t) R
      let P = (block I N (transpose N) R) :> [n][n]t
      in (H, matmul Q P)

  --Hessenberg QR algorithm with Rayleigh quotient shift and deflation
  def eig [n] (A : [n][n]t): ([n][n]t, [n][n]t) = 
    let (H, U) = househess (copy A)
    let epsilon = T.f64 1e-30 --very precise
    let givens (a: t) (b: t): (t,t) =
      let denom = T.(sqrt ((a ** i64 2) + (b ** i64 2)))
      let v = (a T./ denom, b T./ denom)
      in if (T.isnan v.0 || T.isnan v.1) then (T.i64 1, T.i64 0) else v
    let (H,U) = loop (H,U) = (copy H, copy U) for j in 0..<n-1 do
      let m = n - j
      let I = eye m
      in loop (H,U) = (H,U) while (T.abs H[m-1,m-2]) T.> epsilon do
        let sigma = H[m-1,m-1]
        let HmmI = matscale sigma I
        let H = H with [:m,:m] = matsub H[:m,:m] HmmI --Rayleigh quotient shift
        let (cs, ss) = (veczeros m, veczeros m)
        let (H, cs, ss) = loop (H, cs, ss) = (H, cs, ss) for i in 0..<m-1 do
          let (c,s) = givens H[i,i] H[i+1,i] --these must be calculated iteratively, and ONLY IN THIS LOOP!
          let cs = cs with [i] = c
          let ss = ss with [i] = s
          let rotator = [[c,s],[T.neg s,c]]
          let H = H with [i:i+2,i:] = matmul rotator (H[i:i+2,i:] :> [2][]t)
          in (H, cs, ss)
        let (H,U) = loop (H,U) = (H,U) for i in 0..<m-1 do
          let (c,s) = (cs[i], ss[i])
          let rotator = [[c,T.neg s],[s,c]]
          let H = H with [:i+2,i:i+2] = matmul (H[:i+2,i:i+2] :> [][2]t) rotator
          let U = U with [:,i:i+2] = matmul (U[:,i:i+2] :> [][2]t) rotator
          in (H,U)
        let H = H with [:m,:m] = matadd H[:m,:m] HmmI --Rayleigh quotient "un"shift
        in (H,U)
    let D = matdiag H
    --ok now we need to calculate eigenvectors for the uppertriangular matrix H
    --https://math.stackexchange.com/questions/2632460/general-form-of-left-and-right-eigenvectors-of-upper-triangular-matrices
    --I have an example matlab script which does this part in the references (called eigenvectors_of_upper_triangular.m)
    --also hopefully some ppt slides...
    let V = eye n
    let V = loop V = V for i in 2...n do --The columns of V are now the eigenvectors of the Hessenberg form H of A
      let im = i - 1
      let subH = H[:i-1,:i-1] :> [im][im]t
      let subV = V[:i-1,:i-1] :> [im][im]t
      let Hminus = H[:i-1,i-1] :> [im]t
      let lam = D[i-1,i-1]
      let X = matsub (matscale lam subV) (matmul subH subV) --this eigenvector is a linear combo of the previous
      let a = loop a = matones i 1 for j in i-2..>-1 do --now do backsubstitution to solve
        let jp = i-1 - (j+1)
        let prod = if jp < i-1 then dotprod (X[j,j+1:] :> [jp]t) ((copy a[j+1:i-1,0]) :> [jp]t) else T.i64 0
        let v = (Hminus[j] T.- prod) T./ X[j,j]
        in a with [j,0] = if T.isnan v then T.i64 0 else v
      in V with [:i,i-1] = (matmul V[:i,:i] a)[:,0]
    in (D, matmul U V) --THIS ONE!!!

  def matsqrt [n] (A : [n][n]t): [n][n]t =
    -- let (D, E) = eig 30 A --get eigenvalues, vectors
    let (D, E) = eig A --get eigenvalues, vectors
    let sqrtD = map (\i->map (\j->if i==j then T.sqrt D[i,j] else T.i64 0) (0..<n)) (0..<n)
    in matmul (matmul E sqrtD) (inv E)


  def mat [m][n] (v: t) : [m][n]t =
    tabulate_2d m n (\_ _ -> v)

  def i64 [m][n] (v : i64) : [m][n]t =
    tabulate_2d m n (\_ _ -> T.i64 v)

  def f64 [m][n] (v : f64) : [m][n]t =
    tabulate_2d m n (\_ _ -> T.f64 v)

}

-- | ignore

import "dpsolve"
module dps = mk_dpsolve_dense f64

def bellman (x:[2]f64) : [2]f64 =
  [f64.sqrt x[1], f64.sqrt(1 - x[0] ** 2)]

entry test_sa (sa_max:i64) (sa_tol:f64) : ([2]f64, bool, i64, f64) =
  let v0 = [0.5, 0.5]
  let ap = dps.default with sa_max = sa_max
                       with sa_tol = sa_tol
  let {res, conv=b, iter=i, tol, rtol=_} = dps.sa bellman v0 ap 0
  in (res, b, i, tol)

-- ==
-- entry: test_sa
-- input { 60i64 1e-3 } output { [0.785513763, 0.617747547] true 56i64 8.769119278e-4f64 }
-- input { 200i64 1e-9 } output { [0.786151372, 0.618033989] true 186i64 9.120002530e-10f64 }

def bellman_j (a:[2]f64) : ([2]f64, [2][2]f64) =
  let x1 = a[0]
  let x2 = a[1]
  let res = [f64.sqrt x2, f64.sqrt(1-x1**2)]
  let j = [[0                       , 1/(2*f64.sqrt x2) ],
           [-x1/(f64.sqrt(1-x1**2)) , 0                 ]]
  in (res, j)

entry test_poly (sa_max:i64) : ([2]f64, bool, i64, i64, i64, f64) =
  let v0 = [0.5, 0.5]
  let ap = dps.default with sa_max = sa_max
  let {res, jac=_, conv=b, iter_sa=i, iter_nk=j, rtrips=k, tol} = dps.poly bellman_j v0 ap 0
  in (res, b, i, j, k, tol)

-- ==
-- entry: test_poly
-- input { 5i64 } output { [0.786151377, 0.6180339887] true 5i64 4i64 1i64 1.11022302e-16f64 }

def idd n i = tabulate n (f64.bool <-< (==i))

def wrapj [n][m] (f: [n]f64->[m]f64) (x:[n]f64) : ([m]f64,[m][n]f64) =
  (f x, tabulate n (jvp f x <-< idd n) |> transpose)

entry test_poly_jvp (sa_max:i64) : ([2]f64,bool,i64,i64,i64,f64) =
  let v0 = [0.5, 0.5]
  let ap = dps.default with sa_max = sa_max
  let {res, jac=_, conv=b, iter_sa=i, iter_nk=j, rtrips=k, tol} =
    dps.poly (wrapj bellman) v0 ap 0
  in (res, b, i, j, k, tol)

-- ==
-- entry: test_poly_jvp
-- input { 5i64 } output { [0.78615137, 0.618033988] true 5i64 4i64 1i64 1.110223024e-16f64 }

entry test_polyad (sa_max:i64) : ([2]f64,bool,i64,i64,i64,f64) =
  let v0 = [0.5, 0.5]
  let ap = dps.default with sa_max = sa_max
  let {res,conv,iter_sa,iter_nk,rtrips,tol} = dps.polyad bellman v0 ap 0
  in (res,conv,iter_sa,iter_nk,rtrips,tol)

-- ==
-- entry: test_polyad
-- input { 5i64 } output { [0.78615137, 0.618033988] true 5i64 4i64 1i64 1.11022302e-16f64 }

def bellmanr (r:f64) (a:[2]f64) : ([2]f64, [2][2]f64) =
  let f (a:[2]f64) = [f64.sqrt a[1], f64.sqrt(r**2-a[0]**2)]
  let res = f a
  let j = [[0                            , 1/(2*f64.sqrt a[1]) ],
           [-a[0]/f64.sqrt(r**2-a[0]**2) , 0                   ]]
  in (res, j)

-- We then create an entry point that implements an outer map over a
-- call to `dps.poly` with varying radius:

def linspace (n: i64) (start: f64) (end: f64) : [n]f64 =
  tabulate n (\i -> start + f64.i64 i * ((end-start)/f64.i64 n))

entry test_polyr (n:i64) (sa_max:i64) : (bool, i64, [n]f64, [n]f64) =
  let ap = dps.default with sa_max = sa_max
  let rs = linspace n 1 20
  let ress = map (\r -> let v0 = [0.5,0.5]
                        let {res, jac=_, conv=b, iter_sa=i, iter_nk=j, rtrips=_, tol=_} =
                          dps.poly (bellmanr r) v0 ap 0
                        in (r,res[0],b,i+j)) rs
  let converged = reduce (&&) true (map (.2) ress)
  let xs = map (.1) ress
  let iterations = reduce (+) 0 (map (.3) ress)
  in (converged, iterations, rs, xs)

-- ==
-- entry: test_polyr
-- input { 4i64 3i64 } output { true 25i64 [1.0,5.75,10.5,15.25] [0.786151377,2.29601789,3.1641583,3.84163956] }

entry test_poly1d (sa_max : i64) : ([1]f64, [1][1]f64, bool, i64, i64, i64, f64) =
  let ap = dps.default with sa_max = sa_max
  let {res, jac, conv, iter_sa, iter_nk, rtrips, tol} =
    dps.poly (\x -> ([f64.cos x[0]],
                     [[- f64.sin x[0]]]))
             [0.7] ap 0
  in (res,jac,conv,iter_sa,iter_nk,rtrips,tol)

-- ==
-- entry: test_poly1d
-- input { 0i64 } output { [0.739085133] [[-0.6736120230]] true 0i64 3i64 1i64 0.0 }

entry test_sqrt (sa_max : i64) : [1]f64 =
  let ap = dps.default with sa_max = sa_max
  in dps.poly (\x -> ( [ 0.5 * (x[0]+2/x[0]) ],
                       [[ 2*x[0] ]] )
              ) [1.4] ap 0 |> (.res)

-- ==
-- entry: test_sqrt
-- input { 0i64 } output { [1.414213562] }

def dotprod [n] (u:[n]f64) (v:[n]f64) : f64 =
  reduce (+) 0.0 (map2 (*) u v)

def matvecmul [n][m] (A: [n][m]f64) (v: [m]f64) : [n]f64 =
  map (dotprod v) A

def matmul [n][p][m] (us: [n][p]f64) (vs: [p][m]f64) : [n][m]f64 =
  map (matvecmul (transpose vs)) us

def binop [n] (f:f64->f64->f64) (a:[n][n]f64) (b:[n][n]f64) : [n][n]f64 =
  map2 (map2 f) a b

def diag_ex [n] (A:[n][n]f64) : [n]f64 =
  map (\i -> A[i][i]) (iota n)

def diag [n] (a:[n]f64) : [n][n]f64 =
  tabulate_2d n n (\i j -> if i == j then a[i] else 0.0)

def jacobi [n] (A:[n][n]f64) (b:[n]f64) : [n]f64 -> [n]f64 =
  let D' = diag_ex A |> map (1.0/) |> diag -- D^{-1}
  let I = diag (tabulate n (\_ -> 1.0))
  let G = binop (-) I (matmul D' A)
  let f = map (\i -> D'[i][i]*b[i]) (iota n)
  in \x -> map2 (+) (matvecmul G x) f

def eqv_eps [n] (eps:f64) (x:[n]f64) (y:[n]f64) =
  map2 (\a b -> f64.abs(a-b) < eps) x y |> reduce (&&) true

entry test_jacobi [n] (A:[n][n]f64) (b:[n]f64) (k:i64) : [n]f64 =
  let pow f k x = loop x for _i < k do f x
  let f = jacobi A b
  in pow f k (replicate n 1f64)

-- ==
-- entry: test_jacobi
-- input { [[3f64,1,-1],[1.0,-1,2],[-1.0,1,-3]] [1f64,8,-1] 5i64 }
-- output { [2.96707818, -12.2839506, -4.193415637] }
-- input { [[3f64,1,-1],[1.0,-1,2],[-1.0,1,-3]] [1f64,8,-1] 50i64 }
-- output { [3.999950534, -17.9976114, -6.99987789] }
-- input { [[3f64,1,-1],[1.0,-1,2],[-1.0,1,-3]] [1f64,8,-1] 51i64 }
-- output { [3.999960453, -17.9998044, -6.99998946] }

entry test_jacobi_ok : bool =
  let A = [[3f64,1,-1],[1.0,-1,2],[-1.0,1,-3]]
  let b = [1f64,8,-1]
  in eqv_eps 0.0001 (matvecmul A (test_jacobi A b 51)) b

-- ==
-- entry: test_jacobi_ok
-- input { } output { true }

entry test_jacobi_polyad (sa_max: i64) : [3]f64 =
  let A = [[3f64,1,-1],[1.0,-1,2],[-1.0,1,-3]]
  let b = [1f64,8,-1]
  let f = jacobi (copy A) (copy b)
  let x0 = replicate 3 1f64
  let ap = dps.default with sa_max = sa_max
  in dps.polyad f x0 ap 0 |> (.res)

-- ==
-- entry: test_jacobi_polyad
-- input { 2i64 } output { [4.0,-18,-7] }

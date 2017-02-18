-- LocVolCalib
-- ==
-- compiled input @ LocVolCalib-data/small.in
-- output @ LocVolCalib-data/small.out
--
-- compiled input @ LocVolCalib-data/medium.in
-- output @ LocVolCalib-data/medium.out
--
-- compiled input @ LocVolCalib-data/large.in
-- output @ LocVolCalib-data/large.out

import "futlib/math"

default(f32)

fun initGrid(s0: f32, alpha: f32, nu: f32, t: f32, numX: i32, numY: i32, numT: i32)
  : (i32, i32, [numX]f32, [numY]f32, [numT]f32) =
  let logAlpha = f32.log alpha
  let myTimeline = map (\i: f32  -> t * f32 i / (f32 numT - 1.0)) (iota numT)
  let (stdX, stdY) = (20.0 * alpha * s0 * f32.sqrt(t),
                      10.0 * nu         * f32.sqrt(t))
  let (dx, dy) = (stdX / f32 numX, stdY / f32 numY)
  let (myXindex, myYindex) = (i32(s0 / dx), numY / 2)
  let myX = map (\i: f32 -> f32(i) * dx - f32 myXindex * dx + s0) (iota numX)
  let myY = map (\i: f32 -> f32(i) * dy - f32 myYindex * dy + logAlpha) (iota numY)
  in (myXindex, myYindex, myX, myY, myTimeline)

-- make the innermost dimension of the result of size 4 instead of 3?
fun initOperator(x: [n]f32): ([n][]f32,[n][]f32) =
  let dxu     = x[1] - x[0]
  let dx_low  = [[0.0, -1.0 / dxu, 1.0 / dxu]]
  let dxx_low = [[0.0, 0.0, 0.0]]
  let dx_mids = map (\(i: i32): ([]f32,[]f32)  ->
                       let dxl = x[i] - x[i-1]
                       let dxu = x[i+1] - x[i]
                       in ( [ -dxu/dxl/(dxl+dxu), (dxu/dxl - dxl/dxu)/(dxl+dxu),      dxl/dxu/(dxl+dxu) ],
                            [  2.0/dxl/(dxl+dxu), -2.0*(1.0/dxl + 1.0/dxu)/(dxl+dxu), 2.0/dxu/(dxl+dxu) ] )
                    ) (map (+1) (iota(n-2)))
  let (dx_mid, dxx_mid) = unzip dx_mids
  let dxl      = x[n-1] - x[n-2]
  let dx_high  = [[-1.0 / dxl, 1.0 / dxl, 0.0 ]]
  let dxx_high = [[0.0, 0.0, 0.0 ]]
  let dx     = concat (concat dx_low dx_mid) dx_high
  let dxx    = concat (concat dxx_low dxx_mid) dxx_high
  in  (dx, dxx)

fun max(x: f32, y: f32): f32 = if y < x then x else y
fun maxInt(x: i32, y: i32): i32 = if y < x then x else y

fun setPayoff(strike: f32, myX: [numX]f32, _myY: [numY]f32): *[numY][numX]f32 =
  replicate numY (map (\xi: f32  -> max(xi-strike, 0.0)) myX)

-- Returns new myMuX, myVarX, myMuY, myVarY.
fun updateParams(myX:  [numX]f32, myY: [numY]f32, myTimeline: []f32,
                 g: i32, _alpha: f32, beta: f32, nu: f32)
  : ([][]f32, [][]f32, [][]f32, [][]f32) =
  let myMuY  = replicate numX (replicate numY 0.0)
  let myVarY = replicate numX (replicate numY (nu*nu))
  let myMuX  = replicate numY (replicate numX 0.0)
  let myVarX = map (\yj  ->
                      map (\xi: f32  ->
                             f32.exp(2.0*(beta*f32.log(xi) + yj - 0.5*nu*nu*myTimeline[g]))
                          ) myX
                   ) myY
  in  ( myMuX, myVarX, myMuY, myVarY )

fun tridagSeq(a:  [n]f32, b: *[n]f32, c: [n]f32, y: *[n]f32 ): *[n]f32 =
  loop ((y, b)) =
    for 1 <= i < n do
      let beta = a[i] / b[i-1]
      let b[i] = b[i] - beta*c[i-1]
      let y[i] = y[i] - beta*y[i-1]
      in  (y, b)

      let y[n-1] = y[n-1]/b[n-1]
      loop (y) = for (n-1) > i >= 0 do
        let y[i] = (y[i] - c[i]*y[i+1]) / b[i]
        in  y
      in  y

fun tridagPar(a:  [n]f32, b: *[n]f32, c: [n]f32, y: *[n]f32 ): *[n]f32 =
  unsafe
  ----------------------------------------------------
  -- Recurrence 1: b[i] = b[i] - a[i]*c[i-1]/b[i-1] --
  --   solved by scan with 2x2 matrix mult operator --
  ----------------------------------------------------
  let b0   = b[0]
  let mats = map  (\(i: i32): (f32,f32,f32,f32)  ->
                     if 0 < i
                     then (b[i], 0.0-a[i]*c[i-1], 1.0, 0.0)
                     else (1.0,  0.0,             0.0, 1.0))
                  (iota n)
  let scmt = scan (\(a:  (f32,f32,f32,f32))
                   (b: (f32,f32,f32,f32)): (f32,f32,f32,f32)  ->
                     let (a0,a1,a2,a3) = a
                     let (b0,b1,b2,b3) = b
                     let value = 1.0/(a0*b0)
                     in ( (b0*a0 + b1*a2)*value,
                          (b0*a1 + b1*a3)*value,
                          (b2*a0 + b3*a2)*value,
                          (b2*a1 + b3*a3)*value))
                  (1.0,  0.0, 0.0, 1.0) mats
  let b    = map (\(tup: (f32,f32,f32,f32)): f32  ->
                    let (t0,t1,t2,t3) = tup
                    in (t0*b0 + t1) / (t2*b0 + t3))
                 scmt
  ------------------------------------------------------
  -- Recurrence 2: y[i] = y[i] - (a[i]/b[i-1])*y[i-1] --
  --   solved by scan with linear func comp operator  --
  ------------------------------------------------------
  let y0   = y[0]
  let lfuns= map  (\(i: i32): (f32,f32)  ->
                     if 0 < i
                     then (y[i], 0.0-a[i]/b[i-1])
                     else (0.0,  1.0))
                  (iota n)
  let cfuns= scan (\(a: (f32,f32)) (b: (f32,f32)): (f32,f32)  ->
                     let (a0,a1) = a
                     let (b0,b1) = b
                     in ( b0 + b1*a0, a1*b1 ))
                  (0.0, 1.0) lfuns
  let y    = map (\(tup: (f32,f32)): f32  ->
                    let (a,b) = tup
                    in a + b*y0)
                 cfuns
  ------------------------------------------------------
  -- Recurrence 3: backward recurrence solved via     --
  --             scan with linear func comp operator  --
  ------------------------------------------------------
  let yn   = y[n-1]/b[n-1]
  let lfuns= map (\(k: i32): (f32,f32)  ->
                    let i = n-k-1
                    in  if   0 < k
                        then (y[i]/b[i], 0.0-c[i]/b[i])
                        else (0.0,       1.0))
                 (iota n)
  let cfuns= scan (\(a: (f32,f32)) (b: (f32,f32)): (f32,f32)  ->
                     let (a0,a1) = a
                     let (b0,b1) = b
                     in (b0 + b1*a0, a1*b1))
                  (0.0, 1.0) lfuns
  let y    = map (\(tup: (f32,f32)): f32  ->
                    let (a,b) = tup
                    in a + b*yn)
                 cfuns
  let y    = map (\i -> y[n-i-1]) (iota n)
  in y

------------------------------------------/
-- myD,myDD          : [m][3]f32
-- myMu,myVar,result : [n][m]f32
-- RETURN            : [n][m]f32
------------------------------------------/
fun explicitMethod(myD:    [m][3]f32,  myDD: [m][3]f32,
                   myMu:   [n][m]f32,  myVar: [n][m]f32,
                   result: [n][m]f32)
                  : *[n][m]f32 =
  -- 0 <= i < m AND 0 <= j < n
  map (\mu_row var_row result_row: [m]f32  ->
         map (\dx dxx mu var j: f32 ->
                let c1 = if 0 < j
                         then ( mu*dx[0] + 0.5*var*dxx[0] ) * unsafe result_row[j-1]
                         else 0.0
                let c3 = if j < (m-1)
                         then ( mu*dx[2] + 0.5*var*dxx[2] ) * unsafe result_row[j+1]
                         else 0.0
                let c2 =      ( mu*dx[1] + 0.5*var*dxx[1] ) * unsafe result_row[j  ]
                in  c1 + c2 + c3)
             myD myDD mu_row var_row (iota m))
      myMu myVar result

------------------------------------------/
-- myD,myDD     : [m][3]f32
-- myMu,myVar,u : [n][m]f32
-- RETURN       : [n][m]f32
------------------------------------------/
-- for implicitY: should be called with transpose(u) instead of u
fun implicitMethod(myD:  [m][3]f32,  myDD:  [m][3]f32,
                   myMu: [n][m]f32,  myVar: [n][m]f32,
                   u:   *[n][m]f32,  dtInv: f32)
                  : *[n][m]f32 =
  map (\mu_row var_row (u_row: *[]f32): *[m]f32  ->
         let abc = map (\mu var d dd: (f32,f32,f32) ->
                        ( 0.0   - 0.5*(mu*d[0] + 0.5*var*dd[0])
                        , dtInv - 0.5*(mu*d[1] + 0.5*var*dd[1])
                        , 0.0   - 0.5*(mu*d[2] + 0.5*var*dd[2])))
                       mu_row var_row myD myDD
         let (a,b,c) = unzip(abc)
         in if 1==1
            then tridagSeq( a, b, c, u_row )
            else tridagPar( a, b, c, u_row ))
      myMu myVar u

fun rollback
  (_myX: [numX]f32, _myY: [numY]f32, myTimeline: []f32, myResult: *[][]f32,
   myMuX: [][]f32, myDx: [numX][]f32, myDxx: [][]f32, myVarX: [][]f32,
   myMuY: [][]f32, myDy: [numY][]f32, myDyy: [][]f32, myVarY: [][]f32, g: i32)
  : *[numY][numX]f32 =

  let dtInv = 1.0/(myTimeline[g+1]-myTimeline[g])

  -- explicitX
  let u = explicitMethod( myDx, myDxx, myMuX, myVarX, myResult )
  let u = map (\u_row res_row: []f32  ->
                 map (\u_el res_el  -> dtInv*res_el + 0.5*u_el)
                     u_row res_row)
              u myResult

  -- explicitY
  let myResultTR = transpose(myResult)
  let v = explicitMethod( myDy, myDyy, myMuY, myVarY, myResultTR )
  let u = map (\us vs: *[]f32 -> map (+) us vs) u (transpose v)
  -- implicitX
  let u = implicitMethod( myDx, myDxx, myMuX, myVarX, u, dtInv )
  -- implicitY
  let y = map (\u_row v_row: []f32 ->
                 map (\u_el v_el -> dtInv*u_el - 0.5*v_el) u_row v_row)
              (transpose u) v

  let myResultTR = implicitMethod( myDy, myDyy, myMuY, myVarY, y, dtInv )
  in  transpose myResultTR

fun value(numX: i32, numY: i32, numT: i32, s0: f32, strike: f32, t: f32, alpha: f32, nu: f32, beta: f32): f32 =
  let (myXindex, myYindex, myX, myY, myTimeline) =
    initGrid(s0, alpha, nu, t, numX, numY, numT)
  let (myDx, myDxx) = initOperator(myX)
  let (myDy, myDyy) = initOperator(myY)
  let myResult = setPayoff(strike, myX, myY)

  loop (myResult) =
    for (numT-1) > i do
      let (myMuX, myVarX, myMuY, myVarY) =
        updateParams(myX, myY, myTimeline, i, alpha, beta, nu)
      let myResult = rollback(myX, myY, myTimeline, myResult,
                              myMuX, myDx, myDxx, myVarX,
                              myMuY, myDy, myDyy, myVarY, i)

      in myResult
  in myResult[myYindex,myXindex]

fun main (outer_loop_count: i32, numX: i32, numY: i32, numT: i32,
          s0: f32, _strike: f32, t: f32, alpha: f32, nu: f32, beta: f32): []f32 =
  let strikes = map (\i -> 0.001*f32 i) (iota outer_loop_count)
  let res = map (\x -> value(numX, numY, numT, s0, x, t, alpha, nu, beta)) strikes
  in res

-- LocVolCalib
-- ==
-- compiled input @ LocVolCalib-data/small.in
-- output @ LocVolCalib-data/small.out
--
-- notravis input @ LocVolCalib-data/medium.in
-- output @ LocVolCalib-data/medium.out
--
-- notravis input @ LocVolCalib-data/large.in
-- output @ LocVolCalib-data/large.out

default(f32)

fun {int,int,[f32,numX],[f32,numY],[f32,numT]}
  initGrid(f32 s0, f32 alpha, f32 nu, f32 t, int numX, int numY, int numT) =
    let logAlpha = log32(alpha) in
    let myTimeline = map(fn f32 (int i) => t * f32(i) / (f32(numT) - 1.0), iota(numT)) in
    let {stdX, stdY} = {20.0 * alpha * s0 * sqrt32(t),
                        10.0 * nu         * sqrt32(t)} in
    let {dx, dy} = {stdX / f32(numX), stdY / f32(numY)} in
    let {myXindex, myYindex} = {int(s0 / dx), numY / 2} in
    let myX = map(fn f32 (int i) => f32(i) * dx - f32(myXindex) * dx + s0,       iota(numX)) in
    let myY = map(fn f32 (int i) => f32(i) * dy - f32(myYindex) * dy + logAlpha, iota(numY)) in
    {myXindex, myYindex, myX, myY, myTimeline}

-- make the innermost dimension of the result of size 4 instead of 3?
fun {[[f32],n],[[f32],n]} initOperator([f32,n] x) =
    let dxu    = x[1] - x[0] in
    let dxl    = 0.0         in
    let Dxlow  = [[0.0, -1.0 / dxu, 1.0 / dxu]] in
    let Dxxlow = [[0.0, 0.0, 0.0]]              in
    let Dxmids = map(fn {[f32],[f32]} (int i) =>
                       let dxl = x[i] - x[i-1]  in
                       let dxu = x[i+1] - x[i]  in
                       { [ -dxu/dxl/(dxl+dxu), (dxu/dxl - dxl/dxu)/(dxl+dxu),      dxl/dxu/(dxl+dxu) ],
                         [  2.0/dxl/(dxl+dxu), -2.0*(1.0/dxl + 1.0/dxu)/(dxl+dxu), 2.0/dxu/(dxl+dxu) ] }
                   , map (+ (1), iota(n-2))) in
    let {Dxmid, Dxxmid} = unzip(Dxmids)         in
    let dxl    = x[n-1] - x[n-2] in
    let dxu    = 0.0 in
    let Dxhigh = [[-1.0 / dxl, 1.0 / dxl, 0.0 ]] in
    let Dxxhigh= [[0.0, 0.0, 0.0 ]] in
    let Dx     = concat(concat(Dxlow, Dxmid), Dxhigh) in
    let Dxx    = concat(concat(Dxxlow, Dxxmid), Dxxhigh)
    in  {Dx, Dxx}

fun f32 max(f32 x, f32 y) = if y < x then x else y
fun int maxInt(int x, int y) = if y < x then x else y

fun *[[f32,numX],numY] setPayoff(f32 strike, [f32,numX] myX, [f32,numY] myY) =
  let myres = map(fn [f32] (f32 xi) => replicate(numY, max(xi-strike,0.0)), myX) in
  transpose(myres)

-- Returns new myMuX, myVarX, myMuY, myVarY.
fun {[[f32]] , [[f32]] , [[f32]] , [[f32]]}
updateParams( [f32,numX] myX, [f32,numY] myY, [f32] myTimeline,
              int g, f32 alpha, f32 beta, f32 nu    ) =
  let myMuY  = replicate(numX, replicate(numY, 0.0  )) in
  let myVarY = replicate(numX, replicate(numY, nu*nu)) in
  let myMuX  = replicate(numY, replicate(numX, 0.0  )) in
  let myVarX = map( fn [f32] (f32 yj) =>
                      map ( fn f32 (f32 xi) =>
                              exp32(2.0*(beta*log32(xi) + yj - 0.5*nu*nu*myTimeline[g]))
                          , myX )
                  , myY )
  in  { myMuX, myVarX, myMuY, myVarY }

fun *[f32] tridagSeq( [f32,n] a, *[f32,n] b, [f32,n] c, *[f32,n] y ) =
    loop ({y, b}) =
      for 1 <= i < n do
        let beta = a[i] / b[i-1]      in
        let b[i] = b[i] - beta*c[i-1] in
        let y[i] = y[i] - beta*y[i-1]
        in  {y, b}
    in
    let y[n-1] = y[n-1]/b[n-1] in
    loop (y) = for n-1 > i >= 0 do
                 let y[i] = (y[i] - c[i]*y[i+1]) / b[i]
                 in  y
    in  y

fun *[f32] tridagPar( [f32] a, *[f32] b, [f32] c, *[f32] y ) =
    unsafe
    let n    = size(0, a) in
    ----------------------------------------------------
    -- Recurrence 1: b[i] = b[i] - a[i]*c[i-1]/b[i-1] --
    --   solved by scan with 2x2 matrix mult operator --
    ----------------------------------------------------
    let b0   = b[0] in
    let mats = map ( fn {f32,f32,f32,f32} (int i) =>
                         if 0 < i
                         then {b[i], 0.0-a[i]*c[i-1], 1.0, 0.0}
                         else {1.0,  0.0,             0.0, 1.0}
                   , iota(n) ) in
    let scmt = scan( fn {f32,f32,f32,f32} ( {f32,f32,f32,f32} a,
                                                {f32,f32,f32,f32} b ) =>
                         let {a0,a1,a2,a3} = a   in
                         let {b0,b1,b2,b3} = b   in
                         let val = 1.0/(a0*b0)   in
                         { (b0*a0 + b1*a2)*val,
                           (b0*a1 + b1*a3)*val,
                           (b2*a0 + b3*a2)*val,
                           (b2*a1 + b3*a3)*val
                         }
                   , {1.0,  0.0, 0.0, 1.0}, mats ) in
    let b    = map ( fn f32 ({f32,f32,f32,f32} tup) =>
                         let {t0,t1,t2,t3} = tup in
                         (t0*b0 + t1) / (t2*b0 + t3)
                   , scmt ) in
    ------------------------------------------------------
    -- Recurrence 2: y[i] = y[i] - (a[i]/b[i-1])*y[i-1] --
    --   solved by scan with linear func comp operator  --
    ------------------------------------------------------
    let y0   = y[0] in
    let lfuns= map ( fn {f32,f32} (int i) =>
                         if 0 < i
                         then {y[i], 0.0-a[i]/b[i-1]}
                         else {0.0,  1.0            }
                   , iota(n) ) in
    let cfuns= scan( fn {f32,f32} ({f32,f32} a, {f32,f32} b) =>
                         let {a0,a1} = a in
                         let {b0,b1} = b in
                         { b0 + b1*a0, a1*b1 }
                   , {0.0, 1.0}, lfuns ) in
    let y    = map ( fn f32 ({f32,f32} tup) =>
                         let {a,b} = tup in
                         a + b*y0
                   , cfuns ) in
    ------------------------------------------------------
    -- Recurrence 3: backward recurrence solved via     --
    --             scan with linear func comp operator  --
    ------------------------------------------------------
    let yn   = y[n-1]/b[n-1] in
    let lfuns= map ( fn {f32,f32} (int k) =>
                         let i = n-k-1
                         in  if   0 < k
                             then {y[i]/b[i], 0.0-c[i]/b[i]}
                             else {0.0,       1.0          }
                   , iota(n) ) in
    let cfuns= scan( fn {f32,f32} ({f32,f32} a, {f32,f32} b) =>
                         let {a0,a1} = a in
                         let {b0,b1} = b in
                         {b0 + b1*a0, a1*b1}
                   , {0.0, 1.0}, lfuns ) in
    let y    = map ( fn f32 ({f32,f32} tup) =>
                         let {a,b} = tup in
                         a + b*yn
                   , cfuns ) in
    let y    = map (fn f32 (int i) => y[n-i-1], iota(n)) in
    y

------------------------------------------/
-- myD,myDD          : [[f32,3],m]
-- myMu,myVar,result : [[f32,m],n]
-- RETURN            : [[f32,m],n]
------------------------------------------/
fun *[[f32,m],n] explicitMethod( [[f32,3],m] myD,  [[f32,3],m] myDD,
                                  [[f32,m],n] myMu, [[f32,m],n] myVar,
                                  [[f32,m],n] result ) =
  -- 0 <= i < m AND 0 <= j < n
  map( fn [f32] ( {[f32],[f32],[f32]} tup ) =>
         let {mu_row, var_row, result_row} = tup in
         map( fn f32 ({[f32], [f32], f32, f32, int} tup) =>
                let { dx, dxx, mu, var, j } = tup in
                let c1 = if 0 < j
                         then ( mu*dx[0] + 0.5*var*dxx[0] ) * unsafe result_row[j-1]
                         else 0.0 in
                let c3 = if j < (m-1)
                         then ( mu*dx[2] + 0.5*var*dxx[2] ) * unsafe result_row[j+1]
                         else 0.0 in
                let c2 =      ( mu*dx[1] + 0.5*var*dxx[1] ) * unsafe result_row[j  ]
                in  c1 + c2 + c3
            , zip( myD, myDD, mu_row, var_row, iota(m) )
            )
     , zip( myMu, myVar, result ))

------------------------------------------/
-- myD,myDD     : [[f32,3],m]
-- myMu,myVar,u : [[f32,m],n]
-- RETURN       : [[f32,m],n]
------------------------------------------/
-- for implicitY: should be called with transpose(u) instead of u
fun *[[f32]] implicitMethod( [[f32]] myD,  [[f32]] myDD,
                              [[f32]] myMu, [[f32]] myVar,
                             *[[f32]] u,    f32     dtInv  ) =
  map( fn *[f32] ( {[f32],[f32],*[f32]} tup )  =>
         let {mu_row,var_row,u_row} = tup in
         let abc = map( fn {f32,f32,f32} ({f32,f32,[f32],[f32]} tup) =>
                          let {mu, var, d, dd} = tup in
                          { 0.0   - 0.5*(mu*d[0] + 0.5*var*dd[0])
                          , dtInv - 0.5*(mu*d[1] + 0.5*var*dd[1])
                          , 0.0   - 0.5*(mu*d[2] + 0.5*var*dd[2])
                          }
                      , zip(mu_row, var_row, myD, myDD)
                      ) in
         let {a,b,c} = unzip(abc) in
         if 1==1 then tridagSeq( a, b, c, u_row )
                 else tridagPar( a, b, c, u_row )
     , zip(myMu,myVar,u)
     )

fun *[[f32,numX],numY] rollback
    ([f32,numX] myX, [f32,numY] myY, [f32] myTimeline, *[[f32]] myResult,
     [[f32]] myMuX, [[f32]] myDx, [[f32]] myDxx, [[f32]] myVarX,
     [[f32]] myMuY, [[f32]] myDy, [[f32]] myDyy, [[f32]] myVarY, int g) =

    let dtInv = 1.0/(myTimeline[g+1]-myTimeline[g]) in

    -- explicitX
    let u = explicitMethod( myDx, myDxx, myMuX, myVarX, myResult ) in
    let u = map( fn [f32] ({[f32],[f32]} tup) =>
                    let {u_row, res_row} = tup in
                    map (fn f32 ({f32,f32} tup) =>
                           let {u_el,res_el} = tup
                           in  dtInv*res_el + 0.5*u_el
                        , zip(u_row,res_row) )
                , zip(u,myResult) )
    in
    -- explicitY
    let myResultTR = transpose(myResult) in
    let v = explicitMethod( myDy, myDyy, myMuY, myVarY, myResultTR ) in
    let u = map( fn *[f32] ([f32] us, [f32] vs) =>
                   copy(map(+, zip(us, vs)))
               , zip(u, transpose(v))
               ) in
    -- implicitX
    let u = implicitMethod( myDx, myDxx, myMuX, myVarX, u, dtInv ) in
    -- implicitY
    let y = map( fn [f32] ({[f32],[f32]} uv_row) =>
                   let {u_row, v_row} = uv_row in
                   map( fn f32 ({f32,f32} uv) =>
                          let {u_el,v_el} = uv
                          in  dtInv*u_el - 0.5*v_el
                      , zip(u_row,v_row)
                      )
               , zip(transpose(u),v))
    in
    let myResultTR = implicitMethod( myDy, myDyy, myMuY, myVarY, y, dtInv )
    in  transpose(myResultTR)

fun f32 value(int numX, int numY, int numT, f32 s0, f32 strike, f32 t, f32 alpha, f32 nu, f32 beta) =
    let {myXindex, myYindex, myX, myY, myTimeline} =
        initGrid(s0, alpha, nu, t, numX, numY, numT) in
    let {myDx, myDxx} = initOperator(myX) in
    let {myDy, myDyy} = initOperator(myY) in
    let myResult = setPayoff(strike, myX, myY) in

    loop (myResult) =
        for numT-1 > i do
            let {myMuX, myVarX, myMuY, myVarY} =
                updateParams(myX, myY, myTimeline, i, alpha, beta, nu) in
            let myResult = rollback(myX, myY, myTimeline, myResult,
                                    myMuX, myDx, myDxx, myVarX,
                                    myMuY, myDy, myDyy, myVarY, i) in

            myResult in
    myResult[myYindex,myXindex]

fun [f32] main (int outer_loop_count, int numX, int numY, int numT,
                 f32 s0, f32 strike, f32 t, f32 alpha, f32 nu, f32 beta) =
    let strikes = map(fn f32 (int i) => 0.001*f32(i), iota(outer_loop_count)) in
    let res = map(fn f32 (f32 x) => value(numX, numY, numT, s0, x, t, alpha, nu, beta), strikes) in
    res

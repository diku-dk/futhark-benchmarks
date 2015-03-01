fun {int,int,[real],[real],[real]} 
initGrid(real s0, real alpha, real nu, real t, int numX, int numY, int numT) =
    let logAlpha = log(alpha) in
    let myTimeline = map(fn real (int i) => t * toReal(i) / (toReal(numT) - 1.0), iota(numT)) in
    let {stdX, stdY} = {20.0 * alpha * s0 * sqrt(t),
                        10.0 * nu         * sqrt(t)} in
    let {dx, dy} = {stdX / toReal(numX), stdY / toReal(numY)} in
    let {myXindex, myYindex} = {trunc(s0 / dx), numY / 2} in
    let myX = map(fn real (int i) => toReal(i) * dx - toReal(myXindex) * dx + s0,       iota(numX)) in
    let myY = map(fn real (int i) => toReal(i) * dy - toReal(myYindex) * dy + logAlpha, iota(numY)) in
    {myXindex, myYindex, myX, myY, myTimeline}

fun {[[real]],[[real]]} initOperator([real] x) =
    let n = size(0, x) in
    let dxu = x[1] - x[0] in
    let dxl = 0.0 in
    let Dxlow = [[0.0, -1.0 / dxu, 1.0 / dxu]] in
    let Dxxlow = [[0.0, 0.0, 0.0]] in
    let Dxmid = map(fn [real] (int i) => let dxl = x[i] - x[i-1] in
                                         let dxu = x[i+1] - x[i] in
                                         [-dxu/dxl/(dxl+dxu),
                                         (dxu/dxl - dxl/dxu)/(dxl+dxu),
                                         dxl/dxu/(dxl+dxu)],
                   map (op + (1), iota(n-2))) in
    let Dxxmid = map(fn [real] (int i) => let dxl = x[i] - x[i-1] in
                                          let dxu = x[i+1] - x[i] in
                                          [2.0/dxl/(dxl+dxu),
                                          -2.0*(1.0/dxl + 1.0/dxu)/(dxl+dxu),
                                          2.0/dxu/(dxl+dxu)],
                    map (op + (1), iota(n-2))) in
    let dxl = x[n-1] - x[n-2] in
    let dxu = 0.0 in
    let Dxhigh = [[-1.0 / dxl, 1.0 / dxl, 0.0 ]] in
    let Dxxhigh = [[0.0, 0.0, 0.0 ]] in
    let Dx = concat(concat(Dxlow, Dxmid), Dxhigh) in
    let Dxx = concat(concat(Dxxlow, Dxxmid), Dxxhigh) in
    {Dx, Dxx}

fun real max(real x, real y) = if y < x then x else y
fun int maxInt(int x, int y) = if y < x then x else y

fun *[[real]] setPayoff(real strike, [real] myX, [real] myY) =
  let n     = size(0, myY) in
  let myres = map(fn [real] (real xi) => replicate(n, max(xi-strike,0.0)), myX) in
  copy(transpose(myres))

// Returns new myMuX, myVarX, myMuY, myVarY.
fun {[[real]] , [[real]] , [[real]] , [[real]]} 
updateParams( [real] myX, [real] myY, [real] myTimeline, 
	      int g, real alpha, real beta, real nu    ) =
  let { numX, numY } = { size(0,myX), size(0,myY) }    in
  let myMuY  = replicate(numX, replicate(numY, 0.0  )) in
  let myVarY = replicate(numX, replicate(numY, nu*nu)) in
  let myMuX  = replicate(numY, replicate(numX, 0.0  )) in
  let myVarX = map( fn [real] (real yj) => 
		      map ( fn real (real xi) => 
			      exp(2.0*(beta*log(xi) + yj - 0.5*nu*nu*myTimeline[g]))
			  , myX )
		  , myY )
  in  { myMuX, myVarX, myMuY, myVarY }

fun *[real] tridagSeq( [real] a, [real] b, [real] c, *[real] r, *[real] y ) =
    let n   = size(0, a)           in
    let t = copy(replicate(n,0.0)) in
    let y[0] = r[0] in
    let t[0] = b[0] in
    loop ({y, t}) =
      for i < n-1 do
        let i    = i + 1              in
	let beta = a[i] / t[i-1]      in
	let t[i] = b[i] - beta*c[i-1] in
	let y[i] = r[i] - beta*y[i-1] 
	in  {y, t} 
    in
    let y[n-1] = y[n-1]/t[n-1] in
    loop (y) = for j < n - 1 do
                 let i = n - 2 - j in
                 let y[i] = (y[i] - c[i]*y[i+1]) / t[i] 
		 in  y
    in  y

fun *[[real]] explicitMethod( [[real]] myD,  [[real]] myDD, 
			      [[real]] myMu, [[real]] myVar, [[real]] result ) =
  // 0 <= i < m AND 0 <= j < n
  let m = size(0,myMu) in
  copy( map( fn [real] ( {[real],[real],[real]} tup ) =>
	       let {mu_row, var_row, result_row} = tup in  
	       map( fn real ({[real], [real], real, real, int} tup) =>
		      let { dx, dxx, mu, var, j } = tup in
		      let c1 = if 0 < j
		               then ( mu*dx[0] + 0.5*var*dxx[0] ) * result_row[j-1]
			       else 0.0 in
		      let c2 = if j < (m-1)
		               then ( mu*dx[2] + 0.5*var*dxx[2] ) * result_row[j+1]
			       else 0.0 in
		      let c3 = ( mu*dx[1] + 0.5*var*dxx[1] ) * result_row[j]
		      in  c1 + c2 + c3
		  , zip( myD, myDD, mu_row, var_row, iota(m) ) 
		  )
	   , zip( myMu, myVar, result )
	   )
      )

// for implicitY: should be called with transpose(u) instead of u
fun *[[real]] implicitMethod( [[real]] myD,  [[real]] myDD, 
			      [[real]] myMu, [[real]] myVar, 
			     *[[real]] u,   *[[real]] v, real dtInv  ) =
  map( fn *[real] ( {[real],[real],*[real],*[real]} tup )  =>
	 let {mu_row,var_row,u_row,v_row} = tup in
	 let abc = map( fn {real,real,real} ({real,real,[real],[real]} tup) =>
			  let {mu, var, d, dd} = tup in
			  { 0.0   - 0.5*(mu*d[0] + 0.5*var*dd[0])
			  , dtInv - 0.5*(mu*d[1] + 0.5*var*dd[1])
			  , 0.0   - 0.5*(mu*d[2] + 0.5*var*dd[2])
			  }
		      , zip(mu_row, var_row, myD, myDD) 
		      ) in
	 let {a,b,c} = unzip(abc) in
	 tridagSeq( a, b, c, u_row, v_row )
     , zip(myMu,myVar,u,v)
     )
		 
    

////////////////////////////////////////
// code for explicitX:
//   map (\{x_row,res_row}-> map (\{x,res}->0.5*x+dtInv*res, zip(x_row,res_row)), 
//        zip(transpose(myResult),explicitMethod( myDx, myDxx, transpose(myMuX), transpose(myVarX), transpose(myResult) ))
// code for explicitY:
//   explicitMethod(myDy, myDyy, myMuY, myVarY, myResult)

fun *[[real]] rollback
    ([real] myX, [real] myY, [real] myTimeline, *[[real]] myResult,
     [[real]] myMuX, [[real]] myDx, [[real]] myDxx, [[real]] myVarX,
     [[real]] myMuY, [[real]] myDy, [[real]] myDyy, [[real]] myVarY, int g) =

    let {numX, numY} = {size(0, myX), size(0, myY)} in
    let dtInv = 1.0/(myTimeline[g+1]-myTimeline[g]) in

    // explicitX
    let u = explicitMethod( myDx, myDxx, myMuX, myVarX, myResult ) in
    let u = map( fn [real] ({[real],[real]} tup) =>
		    let {u_row, res_row} = tup in
		    map (fn real ({real,real} tup) =>
			   let {u_el,res_el} = tup 
			   in  dtInv*res_el + 0.5*u_el
			, zip(u_row,res_row) )
		, zip(u,myResult) ) 
    in
    // explicitY
    let myResultTR = transpose(myResult) in
    let v = explicitMethod( myDy, myDyy, myMuY, myVarY, myResultTR ) in
    let u = map( fn [real] ({[real],[real]} tup) =>
		    let {u_row, v_row} = tup in
		    map( op +, zip(u_row,v_row) )
	       , zip( u, transpose(v) ) 
	       ) 
    in
    // implicitX
    let u = implicitMethod( myDx, myDxx, myMuX, myVarX, u, u, dtInv ) in

    // implicitY
    let y = copy( map( fn [real] ({[real],[real]} uv_row) =>
			 let {u_row, v_row} = uv_row in
			 map( fn real ({real,real} uv) =>
				let {u_el,v_el} = uv 
				in  dtInv*u_el - 0.5*v_el
			    , zip(u_row,v_row)
			    )
		     , zip(transpose(u),v)
		     ) )
    in
    let myResultTR = implicitMethod( myDy, myDyy, myMuY, myVarY, y, myResultTR, dtInv ) 
    in  transpose(myResultTR)

fun real value(int numX, int numY, int numT, real s0, real strike, real t, real alpha, real nu, real beta) =
    let {myXindex, myYindex, myX, myY, myTimeline} =
        initGrid(s0, alpha, nu, t, numX, numY, numT) in
    let {myDx, myDxx} = initOperator(myX) in
    let {myDy, myDyy} = initOperator(myY) in
    let myResult = setPayoff(strike, myX, myY) in
    
    loop (myResult) =
        for i < numT - 1 do
            let i = numT-2-i in
            let {myMuX, myVarX, myMuY, myVarY} =
                updateParams(myX, myY, myTimeline, i, alpha, beta, nu) in
            let myResult = rollback(myX, myY, myTimeline, myResult,
                                    myMuX, myDx, myDxx, myVarX,
                                    myMuY, myDy, myDyy, myVarY, i) in
            
            myResult in
    myResult[myXindex,myYindex]

fun [real] main (int outer_loop_count, int numX, int numY, int numT, 
		 real s0, real strike, real t, real alpha, real nu, real beta) =
    let strikes = map(fn real (int i) => 0.001*toReal(i), iota(outer_loop_count)) in
    let res = map(fn real (real x) => value(numX, numY, numT, s0, x, t, alpha, nu, beta), strikes) in
    res

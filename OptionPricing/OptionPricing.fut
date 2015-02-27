fun int grayCode(int x) = (x >> 1) ^ x

////////////////////////////////////////
/// Sobol Generator
////////////////////////////////////////
fun int index([int] arr, int i) = arr[i]

fun bool testBit(int n, int ind) =
    let t = (1 << ind) in (n & t) == t

/////////////////////////////////////////////////////////////////
//// INDEPENDENT FORMULA: 
////    filter is redundantly computed inside map.
////    Currently Futhark hoists it outside, but this will
////    not allow fusing the filter with reduce => redomap,
/////////////////////////////////////////////////////////////////
fun int xorInds(int bits_num, int n, [int] dir_vs ) =
    let bits    = iota   ( bits_num )                   in
    let indices = filter ( testBit(grayCode(n)), bits ) in
    reduce( op ^, 0, map( index(dir_vs), indices ) )

fun [int] sobolIndI ( int bits_num, [[int]] dir_vs, int n ) =
    map( xorInds(bits_num, n), dir_vs )

fun [real] sobolIndR( int bits_num, [[int]] dir_vs, int n ) =
    let divisor = 2.0 pow toReal (bits_num)        in
    let arri    = sobolIndI( bits_num, dir_vs, n ) in
        map( fn real (int x) => toReal(x) / divisor, arri )

/////////////////////////////////
//// STRENGTH-REDUCED FORMULA
/////////////////////////////////
fun int index_of_least_significant_0(int n) = 
  let {goon,k} = {True,0} in
  loop ({goon,k,n}) =
        for i < 30 do
	  if(goon) 
	  then if (n & 1) == 1
	       then {True, k+1, n>>1} 
	       else {False,k,   n   }
	  else      {False,k,   n   }
  in k

fun [int] sobolRecI(int bits_num, [[int]] sob_dir_vs, [int] prev, int n) = 
  let bit = index_of_least_significant_0(n) in
  map (fn int ({[int],int} vct_prev) => 
	 let {vct_row, prev} = vct_prev in
	 vct_row[bit] ^ prev
      , zip(sob_dir_vs,prev))

fun [[real]] sobolRecMap(real sob_fact, int bits_num, [[int]] dir_vs, {int,int} lu_bds) =
  let {lb_inc, ub_exc} = lu_bds in 
  let contribs = map( fn [int] (int k) => if (k==0) 
      	       	      	       	       	  then sobolIndI(bits_num,dir_vs,lb_inc+1)
					  else recM(dir_vs,k+lb_inc)
		    , iota(ub_exc-lb_inc) 
		    ) in
  let vct_ints = scan( fn [int] ([int] x, [int] y) => zipWith(op ^, x, y) 
	 	     , replicate( size(0,dir_vs), 0 ) 
		     , contribs
		     ) 
  in  map( fn [real] ([int] xs) => 
	     map ( fn real (int x) => 
		     toReal(x) * sob_fact 
		 , xs)
	 , vct_ints)

fun [int] sobolRecI2([[int]] sob_dirs, [int] prev, int i)=
  let col = recM(sob_dirs, i) in zipWith(op^,prev,col)

fun [int] recM( [[int]] sob_dirs, int i ) =
  let bit= index_of_least_significant_0(i) in
  map( fn int([int] row) => row[bit], sob_dirs )

////////////////////////////////////////
/// Inverse Gaussian
////////////////////////////////////////
fun real polyAppr(      real x,
                        real a0, real a1, real a2, real a3,
                        real a4, real a5, real a6, real a7,
                        real b0, real b1, real b2, real b3,
                        real b4, real b5, real b6, real b7
                    ) =
        (x*(x*(x*(x*(x*(x*(x*a7+a6)+a5)+a4)+a3)+a2)+a1)+a0) /
        (x*(x*(x*(x*(x*(x*(x*b7+b6)+b5)+b4)+b3)+b2)+b1)+b0)

fun real smallcase(real q) =
        q * polyAppr( 0.180625 - q * q,

                      3.387132872796366608,
                      133.14166789178437745,
                      1971.5909503065514427,
                      13731.693765509461125,
                      45921.953931549871457,
                      67265.770927008700853,
                      33430.575583588128105,
                      2509.0809287301226727,

                      1.0,
                      42.313330701600911252,
                      687.1870074920579083,
                      5394.1960214247511077,
                      21213.794301586595867,
                      39307.89580009271061,
                      28729.085735721942674,
                      5226.495278852854561
                    )

fun real intermediate(real r) =
        polyAppr( r - 1.6,

                  1.42343711074968357734,
                  4.6303378461565452959,
                  5.7694972214606914055,
                  3.64784832476320460504,
                  1.27045825245236838258,
                  0.24178072517745061177,
                  0.0227238449892691845833,
                  7.7454501427834140764e-4,

                  1.0,
                  2.05319162663775882187,
                  1.6763848301838038494,
                  0.68976733498510000455,
                  0.14810397642748007459,
                  0.0151986665636164571966,
                  5.475938084995344946e-4,
                  1.05075007164441684324e-9
                )

fun real tail(real r) =
        polyAppr( r - 5.0,

                  6.6579046435011037772,
                  5.4637849111641143699,
                  1.7848265399172913358,
                  0.29656057182850489123,
                  0.026532189526576123093,
                  0.0012426609473880784386,
                  2.71155556874348757815e-5,
                  2.01033439929228813265e-7,

                  1.0,
                  0.59983220655588793769,
                  0.13692988092273580531,
                  0.0148753612908506148525,
                  7.868691311456132591e-4,
                  1.8463183175100546818e-5,
                  1.4215117583164458887e-7,
                  2.04426310338993978564e-5
                )

fun real ugaussianEl( real p ) =
    let dp = p - 0.5
    in  //if  ( fabs(dp) <= 0.425 )
        if ( ( (dp < 0.0 ) && (0.0 - dp <= 0.425) ) || 
	     ( (0.0 <= dp) && (dp <= 0.425)       )  )
        then smallcase(dp)
        else let pp = if(dp < 0.0) then dp + 0.5 
				   else 0.5 - dp       in
             let r  = sqrt( - log(pp) )                in
             let x = if(r <= 5.0) then intermediate(r) 
				  else tail(r)         in
             if(dp < 0.0) then 0.0 - x 
			  else x

// Transforms a uniform distribution [0,1) 
// into a gaussian distribution (-inf, +inf)
fun [real] ugaussian([real] ps) = map(ugaussianEl, ps)


/////////////////////////////////
/// Brownian Bridge
/////////////////////////////////
fun [real] brownianBridgeDates (
                  int    num_dates,
                [[int ]] bb_inds,       // [3,  num_dates]
                [[real]] bb_data,       // [3,  num_dates]
                 [real]  gauss          // [num_dates]
            ) =
    let bi = bb_inds[0] in
    let li = bb_inds[1] in
    let ri = bb_inds[2] in
    let sd = bb_data[0] in
    let lw = bb_data[1] in
    let rw = bb_data[2] in

    let bbrow = copy(replicate(num_dates, 0.0))   in
    let bbrow[ bi[0]-1 ] = sd[0] * gauss[0]       in

    loop (bbrow) =
        for i < num_dates-1 do  // use i+1 since i in 1 .. num_dates-1
            let j  = li[i+1] - 1 in
            let k  = ri[i+1] - 1 in
            let l  = bi[i+1] - 1 in

            let wk = bbrow [k  ] in
            let zi = gauss [i+1] in
            let tmp= rw[i+1] * wk + sd[i+1] * zi in

            let bbrow[ l ] = if( j == -1)
                             then tmp
                             else tmp + lw[i+1] * bbrow[j]
            in  bbrow

        // This can be written as map-reduce, but it
        //   needs delayed arrays to be mapped nicely!
    in loop (bbrow) =
        for ii < num_dates-1 do
            let i = num_dates - (ii+1) in
            let bbrow[i] = bbrow[i] - bbrow[i-1]
            in  bbrow
       in bbrow

// [num_dates,num_paths]
fun [[real]] brownianBridge (
                  int    num_paths,
                  int    num_dates,
                [[int ]] bb_inds,       // [3,  num_dates]
                [[real]] bb_data,       // [3,  num_dates]
                 [real]  gaussian_arr   // [num_paths * num_dates]
            ) =
    let gauss2d  = reshape((num_dates,num_paths), gaussian_arr) in
    let gauss2dT = transpose(gauss2d) in
      transpose( 
	map( brownianBridgeDates(num_dates, bb_inds, bb_data), gauss2dT ) 
      )


/////////////////////////////////
/// Black-Scholes
/////////////////////////////////
fun [real] take(int n, [real] a) = let {first, rest} = split(n, a) in first

fun [real] fftmp(int num_paths, [[real]] md_c, [real] zi) =
    map( fn real (int j) =>
            let x = map( op *, zip(take(j+1,zi), take(j+1,md_c[j])) )
                in  reduce(op +, 0.0, x) 
         , iota(num_paths)
       )

fun [[real]] correlateDeltas(int num_paths, [[real]] md_c, [[real]] zds) =
    map( fftmp(num_paths, md_c), zds )

fun [real] combineVs([real] n_row, [real] vol_row, [real] dr_row) =
    map( op +, zip(dr_row, map( op *, zip(n_row, vol_row ) )))

fun [[real]] mkPrices(  [real]  md_starts, [[real]] md_vols, 
		       [[real]] md_drifts, [[real]] noises ) =
    let e_rows = map( fn [real] ([real] x) => map(exp, x),
                      map(combineVs, zip(noises, md_vols, md_drifts))
                    )
    in  scan( fn [real] ([real] x, [real] y) => map(op *, zip(x, y)), 
	      md_starts, e_rows )

//[num_dates, num_paths]
fun [[real]] blackScholes(
                int      num_paths,
                [[real]] md_c,         //[num_paths, num_paths]
                [[real]] md_vols,      //[num_paths, num_dates]
                [[real]] md_drifts,    //[num_paths, num_dates]
                 [real]  md_starts,    //[num_paths]
                [[real]] bb_arr        //[num_dates,num_paths]
           ) =
    let noises = correlateDeltas(num_paths, md_c, bb_arr)
    in  mkPrices(md_starts, md_vols, md_drifts, noises)

////////////////////////////////////////
// MAIN
////////////////////////////////////////
fun [real] main(
              int        contract_number,
              int        num_mc_it,
              int        num_dates,
              int        num_und,
              int        num_models,
              int        num_bits,
	      int        chunk_size,
             [[int]]   dir_vs,
             [[[real]]] md_cs,
             [[[real]]] md_vols,
             [[[real]]] md_drifts,
             [[real]]   md_sts,
	     [[real]]   md_detvals,
             [[real]]   md_discts,
             [[int]]    bb_inds,
             [[real]]   bb_data) =
  let num_chunks = (num_mc_it + chunk_size - 1)/chunk_size in 

  let chunks = map (fn {int,int} (int i) => 
		      {i*chunk_size, MINint(num_mc_it, (i+1)*chunk_size)}
		   , iota(num_chunks) )
  in
  let payoffs= map( compute_chunk( {contract_number,num_mc_it,num_dates,num_und,num_models,num_bits}, dir_vs,
				   {bb_inds,bb_data}, {md_cs,md_vols,md_drifts,md_sts,md_discts,md_detvals} )
		  , chunks )
  in
  let payoff = reduce ( fn [real] ([real] x, [real] y) => 
			  zipWith(op +, x, y)
		      , replicate(num_models, 0.0)
		      , payoffs )

  in  map (fn real (real price) => price / toReal(num_mc_it), payoff)

//    let sobol_mat = map ( sobolIndR(num_bits, dir_vs), 
//			  map( fn int (int x) => x + 1, iota(num_mc_it) ) ) in
//    let gauss_mat = map ( ugaussian, sobol_mat )                            in
//    let bb_mat    = map ( brownianBridge( num_und, num_dates, bb_inds, bb_data ), gauss_mat ) in
//
//    let payoffs   = map ( fn [real] ([[real]] bb_row) =>
//			    let market_params = zip(md_cs, md_vols, md_drifts, md_sts) in
//			    let bd_row = map (fn [[real]] ({[[real]],[[real]],[[real]],[real]} m) =>
//					             let {c,vol,drift,st} = m in
//						     blackScholes(num_und, c, vol, drift, st, bb_row)
//					     , market_params) 
//			    in
//			    let payoff_params = zip(md_discts, md_detvals, bd_row) 
//			    in  map (fn real ({[real],[real],[[real]]} p) =>
//				       let {disct, detval, bd} = p in
//				       genericPayoff(contract_number, disct, detval, bd)
//				    , payoff_params)
//			, bb_mat)
//    in
//    let payoff    = reduce ( fn [real] ([real] x, [real] y) => 
//			       zipWith(op +, x, y)
//			   , replicate(num_models, 0.0)
//			   , payoffs )
//    in  map (fn real (real price) => price / toReal(num_mc_it), payoff)


fun [real] compute_chunk(
  {int,int,int,int,int,int}           param_ints,
  [[int]]                             dir_vs,
  {[[int]],[[real]]}         brownian_bridge_params,
  {[[[real]]],[[[real]]],[[[real]]],
    [[real]],  [[real]],  [[real]]  } market_params,
  {int,int}                           lu_bds
) = 
    let {contract_number,num_mc_it,num_dates,num_und,num_models,num_bits} = param_ints in
    let {md_cs,md_vols,md_drifts,md_sts,md_discts,md_detvals} = market_params in
    let {bb_inds, bb_data} = brownian_bridge_params in
    let {lb_inc,  ub_exc } = lu_bds in
    let sob_factor= 1.0 / toReal(1 << num_bits) in
    let sobol_mat = if (1 == 1)
    		    then sobolRecMap(sob_factor, num_bits, dir_vs, lu_bds)
		    else map ( sobolIndR(num_bits, dir_vs) 
			     , map( fn int (int x) => x + lb_inc + 1 
			       	  , iota(ub_exc - lb_inc) ) 
			     ) in

    let gauss_mat = map ( ugaussian, sobol_mat ) in
    let bb_mat    = map ( brownianBridge( num_und, num_dates, bb_inds, bb_data ), gauss_mat ) in

    let payoffs   = map ( fn [real] ([[real]] bb_row) =>
			    let market_params = zip(md_cs, md_vols, md_drifts, md_sts) in
			    let bd_row = map (fn [[real]] ({[[real]],[[real]],[[real]],[real]} m) =>
					             let {c,vol,drift,st} = m in
						     blackScholes(num_und, c, vol, drift, st, bb_row)
					     , market_params)
			    in
			    let payoff_params = zip(md_discts, md_detvals, bd_row) 
			    in  map (fn real ({[real],[real],[[real]]} p) =>
				       let {disct, detval, bd} = p in
				       genericPayoff(contract_number, disct, detval, bd)
				    , payoff_params)
			, bb_mat)
    in  reduce ( fn [real] ([real] x, [real] y) => 
		   zipWith(op +, x, y)
	       , replicate(num_models, 0.0)
	       , payoffs )
    

////////////////////////////////////////
// PAYOFF FUNCTIONS
////////////////////////////////////////
fun real genericPayoff(int contract, [real] md_disct, [real] md_detval, [[real]] xss) = 
    if      (contract == 1) then payoff1(md_disct, md_detval, xss)
    else if (contract == 2) then payoff2(md_disct, xss)
    else if (contract == 3) then payoff3(md_disct, xss)
    else 0.0                

fun real payoff1([real] md_disct, [real] md_detval, [[real]] xss) = 
// invariant: xss is a 1x1 matrix
    let detval = md_detval[0]                   in
    let amount = ( xss[0,0] - 4000.0 ) * detval in
    let amount0= if (0.0 < amount) then amount else 0.0
    in  trajInner(amount0, 0, md_disct)

fun real payoff2 ([real] md_disc, [[real]] xss) =
// invariant: xss is a 5x3 matrix
  let {date, amount} = 
    if      (1.0 <= fminPayoff(xss[0])) then {0, 1150.0}
    else if (1.0 <= fminPayoff(xss[1])) then {1, 1300.0}
    else if (1.0 <= fminPayoff(xss[2])) then {2, 1450.0}
    else if (1.0 <= fminPayoff(xss[3])) then {3, 1600.0}
    else let x50  = fminPayoff(xss[4]) in
	 let val  = if      ( 1.0 <= x50 ) then 1750.0
                    else if ( 0.75 < x50 ) then 1000.0
                    else                        x50*1000.0
	 in {4, val}
  in  trajInner(amount, date, md_disc) 

fun real payoff3([real] md_disct, [[real]] xss) =
// invariant: xss is a 367x3 matrix
    let conds  = map (fn bool ([real] x) => (x[0] <= 2630.6349999999998) || 
	 	                            (x[1] <= 8288.0)             || 
		                            (x[2] <=  840.0)
		     , xss)                    in
    let cond  = reduce (op ||, False, conds)   in
    let price1= trajInner(100.0,  0, md_disct) in
    let goto40= cond && 
                  ( (xss[366,0] < 3758.05) || 
                    (xss[366,1] < 11840.0) ||
                    (xss[366,2] < 1200.0 )  )   in
    let amount= if goto40
                  then 1000.0 * fminPayoff(xss[366]) 
                  else 1000.0                   in
    let price2 = trajInner(amount, 1, md_disct) in
    price1 + price2

fun real fminPayoff([real] xs) = 
//    MIN( zipWith(op /, xss, {3758.05, 11840.0, 1200.0}) )
    let {a,b,c} = { xs[0]/3758.05, xs[1]/11840.0, xs[2]/1200.0} in
    if a < b then if a < c then a else c
	     else if b < c then b else c

fun real MIN([real] arr) =
  reduce( fn real (real x, real y) => if(x<y) then x else y, arr[0], arr )

fun int MINint(int x, int y) = if x < y then x else y

fun real trajInner(real amount, int ind, [real] disc) = amount * disc[ind]


//replicate(size(0,dir_vs),0)

// Benchmark running: ./finpar benchmarks/GenericPricing cpp_openmp medium
// futhark -s -fe --in-place-lowering -a -e --compile-sequential OptionPricing.fut > OptionPricing.cpp

//futhark -s -fe --in-place-lowering -a -e --compile-sequential OptionPricing.fut > OptionPricing.cpp
//g++ -O3 -lm OptionPricing.cpp
//./a.out -t timings.txt < OptionPricing.in
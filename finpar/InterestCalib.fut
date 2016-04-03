-- Interest rate calibration
-- ==
-- tags { notravis }
-- compiled input @ InterestCalib-data/small.in
-- output @ InterestCalib-data/small.out
--
-- compiled input @ InterestCalib-data/medium.in
-- output @ InterestCalib-data/medium.out
--
-- compiled input @ InterestCalib-data/large.in
-- output @ InterestCalib-data/large.out

default(f32)

--------------------------------------------------/
--/ ENTRY POINT 
--------------------------------------------------/
fun {{f32,f32,f32,f32,f32,f32},[[f32]]}
    main( int POP, int MCMC_CONV, int l
	    , [[f32]] swaptions,     int ll  
            , [f32]   hermCoefs     
            , [f32]   hermWeights,   int lll   
            , [int ]   sobDirVct
	    ) = 
  let hermData  = zip(hermCoefs, hermWeights) in
  InterestCalibKernel(POP, MCMC_CONV, swaptions, hermData, sobDirVct)

----------------------------------------------------
--/ Prepares the output summary for each swaption
--/ [calibration price, black price, % error]
----------------------------------------------------
fun [[f32]] makeSummary([{f32,f32}] quote_prices) =
  map( fn [f32] ({f32,f32} qp) =>
	 let {black_price, calib_price} = qp in
	 let err_ratio =  (calib_price - black_price) / black_price in
	 [10000.0*calib_price, 10000.0*black_price, 100.0*fabs(err_ratio)]
     , quote_prices
     )

----------------------------------------------------
--/ COMPUTATIONAL KERNEL
----------------------------------------------------
fun {{f32,f32,f32,f32,f32,f32},[[f32]]}
    InterestCalibKernel( int            POP
		       , int            MCMC_CONV
                       , [[f32]]       swaptions
		       , [{f32,f32}]  hermdata
		       , [int]          sobDirVct
		       ) = 
  -- initialize the genomes
  let genomes = map ( fn [f32] (int i) =>
			let k   = 5*i + 1                  in
			let z5s = map ( +k, iota(5) ) in
			let sobs= map ( sobolInd(sobDirVct), z5s )
			in  initGenome( copy(sobs) ) 
		    , iota(POP) ) in
  -- evaluate genomes
  let logLiks = map ( fn f32 ([f32] genome) =>
			let qtprs = map ( evalGenomeOnSwap(genome,hermdata)
					, swaptions ) in 
			let terms = map ( logLikelihood
					, qtprs     ) in
			          reduce( +, 0.0, terms )
		    , genomes ) in
--  logLiks
--  genomes[POP/2]
  let proposals = copy(genomes) in
  let sob_offs = 5*POP+1        in
  loop ({genomes,proposals,logLiks,sob_offs}) =
    for j < MCMC_CONV do
      let rand01   = sobolInd( sobDirVct, sob_offs ) in
      let move_type= selectMoveType(rand01)          in
      let sob_offs = sob_offs + 1                    in

      let {proposals, fb_rats, sob_offs} =
	if (move_type == 1) --  move_type == DIMS_ALL
	then let sob_mat = 
	         map( fn [f32] (int i) =>
			let k   = 5*i + sob_offs          in
			let z5s = map( +k, iota(5) ) in
			map ( sobolInd(sobDirVct), z5s )
		    , iota(POP) ) in
	     let new_gene_rat = 
	         map( mutate_dims_all, zip(sob_mat,genomes,proposals) ) in
	     let {new_genomes, fb_rats} = unzip(new_gene_rat) 
	     in  {new_genomes, fb_rats, sob_offs+5*POP}

	else 
        if (move_type == 2) -- move_type == DIMS_ONE
	then let s1  = sobolInd( sobDirVct, sob_offs )  in
	     let dim_j = int( s1 * f32(5) )        in
	     let sob_mat = 
	         map( fn [f32] (int i) =>
			let k   = 5*i + sob_offs + 1    in
			let z5s = map(+k, iota(5)) in
			map ( sobolInd(sobDirVct), z5s )
		    , iota(POP) ) 
	     in
	     let new_gene_rat = 
	         map( mutate_dims_one(dim_j), zip(sob_mat, genomes, proposals) ) in
	     let {new_genomes, fb_rats} = unzip(new_gene_rat) 
	     in  {new_genomes, fb_rats, sob_offs+5*POP+1}

	else                -- move_type == DEMCMC
	     let new_genomes = 
	         map( fn *[f32] (int i) =>
			let kk  = 8*i + sob_offs            in
			let s1  = sobolInd( sobDirVct, kk ) in
			let k = int( s1 * f32(POP-1) ) in -- random in [0,POP-1)
			let {k,cand_UB} = if k == i 
			                  then {POP-1, POP-2}
					  else {k,     POP-1} 
			in
			let s2  = sobolInd(sobDirVct, kk+1) in
			let l = int( s2*f32(cand_UB) ) in -- random in [0,cand_UB -1)
			let l = if (l == i) || (l == k)
			        then cand_UB
				else l
		        in
			let s3      = sobolInd(sobDirVct, kk+2)       in
			let z5s     = map( +(kk+3),      iota(5) ) in
			let sob_row = map( sobolInd(sobDirVct), z5s ) in
		            mcmc_DE(s3, sob_row, genomes[i], genomes[k], genomes[l])
		    , iota(POP) ) 
	     in  {new_genomes, replicate(POP, 1.0), sob_offs+8*POP}
        in
      let new_logLiks = 
	  map ( fn f32 ([f32] genome) =>
		    let qtprs = map ( evalGenomeOnSwap(genome,hermdata)
				    , swaptions ) in 
		    let terms = map ( logLikelihood
				    , qtprs     ) in
		    reduce( +, 0.0, terms )
	      , proposals ) in
      let res_gene_liks = 
	  map( fn {*[f32],f32} ({[f32],f32,[f32],f32,f32,int} tup) =>
		 let {gene, logLik, new_gene, new_logLik, fb_rat, i} = tup    in
		 let acceptance = min( 1.0, exp32(new_logLik - logLik)*fb_rat ) in
		 let rand01     = sobolInd( sobDirVct, sob_offs+i )           in       
		 let {res_gene, res_logLik} = 
		       if ( rand01 < acceptance ) 
                       then {new_gene, new_logLik}
		       else {gene,     logLik    }
		 in {copy(res_gene), res_logLik}
             , zip(genomes, logLiks, proposals, new_logLiks, fb_rats, iota(POP))
	     )
      in
      let {res_genomes, res_logLiks} = unzip(res_gene_liks) in
      {res_genomes, proposals, res_logLiks, sob_offs+POP}
  -- END OF DO LOOP!!!

  in
  let {winner_ind, winner_logLik} = 
      reduce( fn {int,f32} ({int,f32} t1, {int,f32} t2) =>
		let {i1, v1} = t1 in let {i2, v2} = t2 in
		if (v1 < v2) then {i2, v2} else {i1, v1}
	    , {0, -infinity()}
	    , zip( iota(POP), logLiks ) 
	    )
  in
  let winner = genomes[winner_ind] in
  let winner_quote_prices = 
      map ( evalGenomeOnSwap(genomes[winner_ind],hermdata)
	  , swaptions ) 
  in  { {winner[0],winner[1],winner[4],winner[3],winner[2],winner_logLik}
      , makeSummary(winner_quote_prices)
      }

--------------------------------------------------/
--/ Constants and Utility Functions
--------------------------------------------------/
fun f32 EPS0   () = 1.0e-3
fun f32 EPS    () = 1.0e-5
fun f32 PI     () = 3.1415926535897932384626433832795

fun bool IS_CAUCHY_LLHOOD  () = True 
fun f32 LLHOOD_CAUCHY_OFFS() = 5.0
fun f32 LLHOOD_NORMAL_OFFS() = 1.0

fun f32 r       () = 0.03
fun f32 infinity() = 1.0e49
fun f32 epsilon () = EPS()

fun int  itMax   () = 10000


fun f32 min(f32 a, f32 b) = if(a < b) then a else b
fun f32 max(f32 a, f32 b) = if(a < b) then b else a

fun int minI(int a, int b) = if(a < b) then a else b
fun int maxI(int a, int b) = if(a < b) then b else a

fun f32 fabs(f32 a) = if(a < 0.0) then -a else a

fun bool equal(f32 x1, f32 x2) =
    fabs(x1-x2) <= 1.0e-8

--------------------------------------------------/
--/ evaluating a genome on a swaption and getting
--/ back the quote and the estimated price
--------------------------------------------------/
fun {f32,f32} evalGenomeOnSwap (   
                        [f32]         genomea
		      , [{f32,f32}]  hermdata
		      , [f32]         swaption 
		) = 
  let {a,b,rho,nu,sigma} = {genomea[0],genomea[1],genomea[2],genomea[3],genomea[4]} in
  let swap_freq  = swaption[1] in
  let maturity   = add_years( today(), swaption[0] ) in
  let n_schedi   = int(12.0 * swaption[2] / swap_freq) in

  let tmat0      = date_act_365( maturity, today() ) in

  ----------------------------------------
  -- Quote (black) price computation
  -- This computation does not depend on 
  --   the swaption and can be separated
  --   and hoisted outside the swaption
  --   and convergence loop ...
  ----------------------------------------
  let a12s = map ( fn {f32,f32,f32} (int i) =>
		     let a1 = add_months( maturity, swap_freq*f32(i) ) in
		     let a2 = add_months( a1, swap_freq ) in
		     { zc(a2) * date_act_365(a2, a1), a1, a2 }
		 , iota(n_schedi) ) in
  let {lvl, t0, tn} = reduce( fn {f32,f32,f32} ( {f32,f32,f32} tup1,
						    {f32,f32,f32} tup2 ) =>
				let {lvl,t0,tn} = tup1 in
				let {a12,a1,a2} = tup2 in
				{ lvl + a12, min(t0,a1), max(tn,a2) }
			    , {0.0, max_date(), min_date()}
			    , a12s ) in

  let strike     = ( zc(t0) - zc(tn) ) / lvl in
  let d1         = 0.5 * swaption[3] * tmat0 in
  let new_quote  = lvl * strike * ( uGaussian_P(d1) - uGaussian_P(-d1) ) in

  -- starting new price compuation
  let {v0_mat, dummy1, dummy2} = bigv( {a,b,rho,nu,sigma}, tmat0 ) in
  let mux = 0.0 - bigmx( {a,b,rho,nu,sigma}, today(), maturity, today(), maturity ) in
  let muy = 0.0 - bigmy( {a,b,rho,nu,sigma}, today(), maturity, today(), maturity ) in
  let zc_mat = zc(maturity) in
  let sqrt_bfun_a = sqrt32( b_fun(2.0*a, tmat0) ) in
  let sqrt_bfun_b = sqrt32( b_fun(2.0*b, tmat0) ) in
  let rhoxy  = rho * b_fun(a+b, tmat0) / (sqrt_bfun_a * sqrt_bfun_b) in
  let sigmax = sigma * sqrt_bfun_a  in
  let sigmay = nu    * sqrt_bfun_b  in
  --
  let rhoxyc = 1.0 - rhoxy * rhoxy  in -- used in reduction kernel
  let rhoxycs= sqrt32( rhoxyc )       in -- used in reduction kernel
  let sigmay_rhoxycs = sigmay * rhoxycs  in
  let t4     = (rhoxy * sigmay) / sigmax in

  -- computing n_schedi-size temporary arrays
  let tmp_arrs = 
          map( fn {f32,f32,f32,f32,f32,f32,f32} (int i) =>
		 let beg_date = add_months( maturity, swap_freq*f32(i) ) in 
                 let end_date = add_months( beg_date, swap_freq  )          in 
		 let res      = date_act_365( end_date, beg_date ) * strike in
		 let cii      = if i==(n_schedi-1) then 1.0 + res  else res in

		 let date_tod1= date_act_365(end_date, today()) in
		 let {v0_end,dummy1,dummy2} = bigv( {a,b,rho,nu,sigma}, date_tod1 ) in

		 let date_tod2= date_act_365(end_date, maturity) in
		 let {vt_end, baii, bbii  } = bigv( {a,b,rho,nu,sigma}, date_tod2 ) in

		 let expo_aici = 0.5 * (vt_end - v0_end + v0_mat) in
		 let fact_aici = cii * zc(end_date) / zc_mat      in

		     { baii
		     , bbii
		     , fact_aici * exp32( expo_aici )
		     , log32( fact_aici ) + expo_aici
		     , 0.0  - ( baii + bbii * t4 )
		     , fact_aici
		     , bbii * (mux * t4 - (muy - 0.5*rhoxyc*sigmay*sigmay*bbii) ) + expo_aici
		     }
             , iota(n_schedi) ) in
  let {bas, bbs, aicis, log_aicis, scales, cs, t1_cs} = unzip(tmp_arrs) in
  let scals = {b, sigmax, sigmay, rhoxy, rhoxyc, rhoxycs, mux, muy}     in
  let exact_arrs = zip( bas, bbs, aicis, log_aicis )                    in

  -- exactYhat via Brent method 
  let eps = 0.5 * sigmax in
  let f   = exactYhat( n_schedi, scals, exact_arrs, mux       ) in
  let g   = exactYhat( n_schedi, scals, exact_arrs, mux + eps ) in
  let h   = exactYhat( n_schedi, scals, exact_arrs, mux - eps ) in

  -- integration with Hermite polynomials
  let herm_arrs   = zip( bbs, scales, cs, t1_cs ) in
  let df          = 0.5 * ( g - h ) / eps    in
  let sqrt2sigmax = sqrt32(2.0) * sigmax       in
  let t2          = rhoxy / (sigmax*rhoxycs) in

  let accums = map( fn f32 ({f32,f32} herm_el) =>
		      let {x_quad, w_quad} = herm_el      in
		      let x  = sqrt2sigmax * x_quad + mux in
		      let yhat_x = f + df*(x - mux)       in
		      let h1 = ( (yhat_x - muy) / sigmay_rhoxycs ) - t2*( x - mux ) in
		      let accum1s = map( fn f32 ({f32,f32,f32,f32} tup) =>
					   let {bbi, scalei, csi, t1_csi} = tup in
					   let h2 = h1 + bbi * sigmay_rhoxycs   in
					   let expo_aici = t1_csi + scalei*x    in
					   let fact_aici = csi                  in
					   let expo_part = uGaussian_P_withExpFactor( -h2, expo_aici )
					   in  fact_aici * expo_part
				       , zip( bbs, scales, cs, t1_cs )
				       ) in
		      let accum1 = reduce( +, 0.0, accum1s ) in 
		      let tmp    = sqrt32(2.0) * x_quad           in
                      let t1     = exp32( - 0.5 * tmp * tmp )     in
		      w_quad * t1 * ( uGaussian_P(-h1) - accum1 )
                  , hermdata )
  in
  let accum = reduce( +, 0.0, accums ) in
  let new_price = zc_mat * ( accum / sqrt32( PI() ) ) in
  {new_quote, new_price}


--------------------------------------------------/
--/ Sobol Random Number Generation
--------------------------------------------------/
--fun [f32] getChunkSobNums(int sob_ini, int CHUNK, [int] dirVct) = 
--  let norm_fact = 1.0 / ( f32(1 << size(0,dirVct)) + 1.0 ) in
--  let sob_inds  = map(+sob_ini, iota(CHUNK))
--  in  map( sobolInd(dirVct,norm_fact), sob_inds )

fun f32 sobolInd( [int] dirVct, int n ) =
    -- placed norm_fact here to check that hoisting does its job!
    let norm_fact = 1.0 / ( f32(1 << size(0,dirVct)) + 1.0 ) in
    let n_gray = (n >> 1) ^ n in
    let res = 0 in
    loop (res) =
      for i < size(0,dirVct) do
        let t = 1 << i in
        if (n_gray & t) == t
	then res ^ dirVct[i]
	else res
    in  f32(res) * norm_fact
  
--------------------------------------------------/
--/ Genome Implementation
--------------------------------------------------/
fun [{f32,f32,f32}] genomeBounds() =
    [ {EPS0(),     1.0-EPS0(), 0.02}
    , {EPS0(),     1.0-EPS0(), 0.02}
    , {EPS0()-1.0, 1.0-EPS0(), 0.0 }
    , {EPS0(),     0.2,        0.01}
    , {EPS0(),     0.2,        0.04}
    ]

fun [f32] initGenome ([f32] rand_nums) =
  map (fn f32 ({f32, {f32,f32,f32}} tup) =>
		    let {r01, {g_min, g_max, g_ini}} = tup 
		    in  r01*(g_max - g_min) + g_min
		    
	       , zip(rand_nums, genomeBounds())
	       ) 

fun int selectMoveType(f32 r01) = 
  if r01 <= 0.2   then 1 -- r01 in [0.0, 0.2] => DIMS_ALL
  else 
    if r01 <= 0.5 then 2 -- r01 in (0.2, 0.5] => DIMS_ONE
    else               3 -- r01 in (0.5, 1.0) => DEMCMC
--{ MV_EL_TYPE(0.2,DIMS_ALL), MV_EL_TYPE(0.5,DIMS_ONE), MV_EL_TYPE(1.0,DEMCMC) };

fun f32 MOVES_UNIF_AMPL_RATIO() = 0.005

fun {*[f32],f32} mutate_dims_all({[f32],[f32],[f32]} tup) = 
  let {sob_row, orig, muta} = tup in
  let gene_bds = genomeBounds()   in
  let amplitude = MOVES_UNIF_AMPL_RATIO() in
  let gene_rats = map( mutateHelper(amplitude), zip(sob_row,orig,muta,gene_bds) ) in
  let {tmp_genome, fb_rats} = unzip(gene_rats) in
  let new_genome= map( constrainDim, zip(tmp_genome, gene_bds) ) in
  let fb_rat    = reduce(*, 1.0, fb_rats)
  in  {copy(new_genome), fb_rat}
  
fun {*[f32],f32} mutate_dims_one(int dim_j, {[f32],[f32],[f32]} tup) = 
  let {sob_row, orig, muta} = tup in
  let gene_bds = genomeBounds()   in
  let amplitudes= map(fn f32 (int i) =>
			  if i == dim_j then MOVES_UNIF_AMPL_RATIO() else 0.0
		     , iota(size(0,orig)) )
  in  
  let gene_rats = map( mutateHelper, zip(amplitudes,sob_row,orig,muta,gene_bds) ) in
  let {tmp_genome, fb_rats} = unzip(gene_rats) in
  let new_genome= map( constrainDim, zip(tmp_genome, gene_bds) ) in
  let fb_rat    = reduce(*, 1.0, fb_rats)
  in  {copy(new_genome), fb_rat}


fun *[f32] mcmc_DE(f32 r01, [f32] sob_row, [f32] g_i, [f32] g_k, [f32] g_l) =
  let gene_bds = genomeBounds()         in
  let gamma_avg = 2.38 / sqrt32(2.0*5.0)    in
  let ampl_ratio= 0.1 * MOVES_UNIF_AMPL_RATIO() in
  let gamma1    = gamma_avg - 0.5 + r01 in
  let mm_diffs  = map( fn f32 ({f32,f32,f32} tup) => 
		           let {g_min, g_max, uu} = tup 
			   in  g_max - g_min
		     , gene_bds )
  in 
  let tmp_genome = zipWith( perturbation(gamma1,ampl_ratio) 
			  , g_i, g_k, g_l, sob_row, mm_diffs  )
 
  in  copy( map( constrainDim, zip(tmp_genome, gene_bds) ) )
  

fun f32 perturbation( f32 gamma1, f32 ampl_rat, f32 gene,
		       f32 gene_k, f32 gene_l,   f32 r01, f32 mm_diff ) =
  let amplitude     = fabs( mm_diff * ampl_rat ) in
  let semiamplitude = amplitude / 2.0            in
  let perturb       = ( amplitude * r01 - semiamplitude ) 
  in  gene + perturb + gamma1 * ( gene_k - gene_l )

fun {f32,f32} mutateHelper( f32 ampl_ratio, f32 r01, f32 gene, f32 prop, {f32,f32,f32} gmm) =
  let {g_min, g_max, g_ini} = gmm in
  let amplitude     = fabs( (g_max - g_min) * ampl_ratio ) in
  let semiamplitude = amplitude / 2.0 in
  
  let tmp_min_max = min( g_max, gene + semiamplitude ) in
  let tmp_max_min = max( g_min, gene - semiamplitude ) in
  let forward_range = tmp_min_max - tmp_max_min        in

  let tmp_min_max = min( g_max, prop + semiamplitude ) in
  let tmp_max_min = max( g_min, prop - semiamplitude ) in
  let backward_range = tmp_min_max - tmp_max_min            in

  let bf_fact = if 0.0 < semiamplitude 
		then (backward_range / forward_range) 
		else 1.0
  in {gene + amplitude * r01 - semiamplitude, bf_fact}


fun f32 constrainDim( f32 gene, {f32,f32,f32} tup ) =
  let {g_min, g_max, g_ini} = tup 
  in  max( g_min, min(g_max, gene) )

--------------------------------------------------------------/
--/ Likelihood implementation
--------------------------------------------------------------/
fun f32 logLikelihood(f32 y_ref, f32 y) =
  if   IS_CAUCHY_LLHOOD() 
  then logLikeCauchy(y_ref, y)
  else logLikeNormal(y_ref, y)

fun f32 normalPdf( f32 z, f32 mu, f32 sigma ) =
    let sigma  = fabs(sigma) in 
    let res    = 1.0 / (sigma * sqrt32(2.0*PI())) in
    let ecf    = (z-mu) * (z-mu) / (2.0 * sigma * sigma) in
    res * exp32( 0.0 - ecf )
fun f32 logLikeNormal( f32 y_ref, f32 y) =
    let sigma = (y_ref / 50.0) * LLHOOD_NORMAL_OFFS() in
    let pdfs  = normalPdf( y, y_ref, sigma ) in
    log32(pdfs + 1.0e-20)

fun f32 cauchyPdf( f32 z, f32 mu, f32 gamma ) = -- mu=0.0, gamma=4.0
    let x = (z-mu) / gamma in
    1.0 / ( PI() * gamma * (1.0 + x*x) )
fun f32 logLikeCauchy( f32 y_ref, f32 y ) = 
    let gamma = ( fabs(y_ref) / 50.0 ) * LLHOOD_CAUCHY_OFFS() + 0.01 in
    let pdfs  = cauchyPdf( y, y_ref, gamma ) in
    log32(pdfs + 1.0e-20)

----------------------------------------------------------------
----/ MATH MODULE
----------------------------------------------------------------

---------------------------------------------------------------------------
-- Cumulative Distribution Function for a standard normal distribution

fun f32 uGaussian_P(f32 x) =
    let u = x / sqrt32(2.0) in
    let e = if (u < 0.0) then -erf(-u)
                         else  erf( u)
    in 0.5 * (1.0 + e)

fun f32 uGaussian_P_withExpFactor( f32 x, f32 exp_factor ) =
    let u   = fabs( x / sqrt32(2.0) ) in
    let e   = erff_poly_only(u)     in
    let res = 0.5 * e * exp32(exp_factor-u*u) in
    if ( 0.0 <= x )
    then exp32(exp_factor) - res
    else res

---------------------------------------------------------------------------
-- polynomial expansion of the erf() function, with error<=1.5e-7
--   formula 7.1.26 (page 300), Handbook of Mathematical Functions, Abramowitz and Stegun
--   http:--people.math.sfu.ca/~cbm/aands/frameindex.htm

fun f32 erf(f32 x) =
    let p = 0.3275911 in
    let {a1,a2,a3,a4,a5} =
        {0.254829592, -0.284496736, 1.421413741, -1.453152027, 1.061405429} in
    let t  = 1.0/(1.0+p*x)  in
    let t2 = t  * t         in
    let t3 = t  * t2        in
    let t4 = t2 * t2        in
    let t5 = t2 * t3        in
         1.0 - (a1*t + a2*t2 + a3*t3 + a4*t4 + a5*t5) * exp32(-(x*x))

---------------------------------------------------------------------------
-- iteration_max = 10000 (hardcoded)

fun f32 erff_poly_only( f32 x ) =
    let p = 0.3275911 in
    let {a1,a2,a3,a4,a5} = 
        {0.254829592, -0.284496736, 1.421413741, -1.453152027, 1.061405429} in

    let t  = 1.0/(1.0+p*x) in
    let t2 = t  * t        in
    let t3 = t  * t2       in
    let t4 = t2 * t2       in
    let t5 = t2 * t3       in
    (a1*t + a2*t2 + a3*t3 + a4*t4 + a5*t5)

------------------------------------------------------------------------
-- if fid = 33 then: to_solve(f32 x) = (x+3.0)*(x-1.0)*(x-1.0)
-- otherwise follows the f32 implementation
------------------------------------------------------------------------

fun f32 to_solve(int fid, [{f32,f32}] scalesbbi, f32 yhat) =
    if(fid == 33) then (yhat+3.0)*(yhat-1.0)*(yhat-1.0)
    else
        let tmps = map( fn f32 ( {f32,f32} scalesbbi ) =>
                            let {scales, bbi} = scalesbbi in
                                scales * exp32(-bbi*yhat)
                        , scalesbbi
                      )
            in reduce(+, 0.0, tmps) - 1.0

--------------------------------------------------------/
---- the function-parameter to rootFinding_Brent
--------------------------------------------------------/


fun {f32,int,f32}
rootFinding_Brent(int fid, [{f32,f32}] scalesbbi, f32 lb, f32 ub, f32 tol, int iter_max) =
    let tol      = if(tol     <= 0.0) then 1.0e-9 else tol      in
    let iter_max = if(iter_max<= 0  ) then 10000  else iter_max in
    let {a,b}    = {lb,ub}                                      in

    -- `to_solve' is curried so it will need extra arguments!!!
    -- said fid refers to the version of `to_solve', e.g., testing or f32 implem.
    -- Was previous (eval(a),eval(b)), now:
    let {fa,fb}  = { to_solve(fid, scalesbbi, a), to_solve(fid, scalesbbi, b) } in

    if(0.0 <= fa*fb)
    then
        if(0.0 <= a) then { 0.0, 0,  infinity() }  -- root not bracketed above
                     else { 0.0, 0, -infinity() }  -- root not bracketed below
    else
    let {fa, fb, a, b} = if( fabs(fa) < fabs(fb) ) 
			 then {fb, fa, b, a}
			 else {fa, fb, a, b} in
    let {c,fc}    = {a, fa} in
    let mflag     = True    in
    let it        = 0       in
    let d         = 0.0     in

    loop ({a,b,c,d,fa,fb,fc,mflag,it}) =
        for i < iter_max do

            if( fb==0.0 || fabs(b-a)<tol )
            then {a,b,c,d,fa,fb,fc,mflag,it}
            else

                -- the rest of the code implements the else branch!

                let s = if( fa==fc || fb == fc )
                        then    b-fb*(b-a)/(fb-fa)

                        else let s1 = (a*fb*fc)/( (fa-fb)*(fa-fc) ) in
                             let s2 = (b*fa*fc)/( (fb-fa)*(fb-fc) ) in
                             let s3 = (c*fa*fb)/( (fc-fa)*(fc-fb) ) in
                                s1 + s2 + s3
                                                                    in

                let {mflag, s} = if ( ( !((3.0*a+b)/4.0 <= s && s  <= b)    ) ||
                                      (     mflag && fabs(b-c)/2.0 <= fabs(s-b) ) ||
                                      ( !mflag && fabs(c-d)/2.0    <= fabs(s-b) ) ||
                                      (     mflag && fabs(b-c)     <= fabs(tol) ) ||
                                      ( !mflag && fabs(c-d)        <= fabs(tol) )
                                    )
                                 then {True,  (a+b)/2.0}
                                 else {False, s        }
                                                                    in
                -- Was previous `eval(s)', Now:
                let fs = to_solve(fid, scalesbbi, s)                in

                -- d is assigned for the first time here:
                -- it's not used above because mflag is set
                let d = c in let {c,fc} = {b, fb}                   in
                let {a,b,fa,fb} = if( fa*fs < 0.0 )
                                  then {a,s,fa,fs}
                                  else {s,b,fs,fb}                  in

                let {a,b,fa,fb} = if( fabs(fa) < fabs(fb) )
                                  then {b,a,fb,fa} -- swap args
                                  else {a,b,fa,fb}                  in

                -- reporting non-convergence!
                let dummy =
                    if(i == iter_max-1)
                    then -- # ERROR: Brent method not converged
                         0
                    else 0

                in {a,b,c,d,fa,fb,fc,mflag,i}

    -- Finally, the result of function rootFinding_Brent is:
    in { b, it, fb }


----------------------------------------------------------------
----/ G2PP Module
----------------------------------------------------------------

fun f32 zc(f32 t) = exp32(-r() * date_act_365(t, today()))

------------------------------------------/
---- the first param `swaption' is a triple of f32s,
----    i.e., the swaption maturity date, frequency of payment and
----          swapt-term in years (how often to vary the condition of the contract)
----  the result is also a triple:
----    the maturity time stamp,
----    the range of time stamps of for each swap term : [(f32,f32)]
----    the strike price
------------------------------------------/
-- Quote (block) price computation
------------------------------------------/
fun {f32,[{f32,f32}],{f32,f32}}
extended_swaption_of_swaption({f32,f32,f32} swaption)  =  -- swaption = (sw_mat, freq, sw_ty)
    let {sw_mat, freq, sw_ty} = swaption          in
    let maturity   = add_years( today(), sw_mat ) in
    let nschedule  = int(12.0 * sw_ty / freq)   in

    let a12s = map ( fn {f32,f32,f32} (int i) =>
		     let a1 = add_months( maturity, freq*f32(i) ) in
		     let a2 = add_months( a1, freq ) in
		     { zc(a2) * date_act_365(a2, a1), a1, a2 }
		 , iota(nschedule) ) in

    let {lvl, t0, tn} = reduce( fn {f32,f32,f32} ( {f32,f32,f32} tup1,
						    {f32,f32,f32} tup2 ) =>
				let {lvl,t0,tn} = tup1 in
				let {a12,a1,a2} = tup2 in
				{ lvl + a12, min(t0,a1), max(tn,a2) }
			    , {0.0, max_date(), min_date()}
			    , a12s ) in

    let {lvls, a1s, a2s} = unzip( a12s )     in
    let swap_sched       = zip  ( a1s, a2s ) in
    let strike     = (zc(t0) - zc(tn)) / lvl

    in {maturity, swap_sched, {strike,lvl}}


fun f32 b_fun(f32 z, f32 tau) = (1.0-exp32(-z*tau))/z

fun f32 t_fun(f32 sigma, f32 x, f32 tau) =
    let expxtau  = exp32(-x*tau)     in
    let exp2xtau = expxtau*expxtau in
        sigma*sigma/(x*x)*(tau+2.0/x*expxtau-1.0/(2.0*x)*exp2xtau-3.0/(2.0*x))


------------------------------------------------------------------/
-- the first parameter `genome' is the five-genes genome used in
--     the genetic algorithms that models the interest rate
-- the second parameter is the time
-- the result is V in Brigo and Mercurio's book page 148.
--     \var(\int_t^T [x(u)+y(u)]du)
------------------------------------------------------------------/
fun {f32,f32,f32} bigv({f32,f32,f32,f32,f32} genome, f32 tau) =
    let {g_a, g_b, g_rho, g_nu, g_sigma} = genome in

    -- sanity check; this check should be hoisted higher up
    let g_sigma = if(g_sigma == 0.0) then 1.0e-10 else g_sigma in

    let ba = b_fun(g_a,        tau) in
    let bb = b_fun(g_b,        tau) in
    let t1 = t_fun(g_sigma,g_a,tau) in
    let t2 = t_fun(g_nu,   g_b,tau) in

    let t3 = 2.0 * g_rho * g_nu * g_sigma / (g_a * g_b)*
             ( tau - ba - bb + b_fun(g_a+g_b, tau) )

        in {t1+t2+t3, ba, bb}

------------------------------------------------------------------/
-- the first parameter `genome' is the five-genes genome used in
--     the genetic algorithms that models the interest rate
-- the other parameter are times: today, maturity, and the
--      lower and upper bound of the considered time interval
--
-- the result is: x drift term in tmat-forward measure
------------------------------------------------------------------/

fun f32 bigmx( {f32,f32,f32,f32,f32} genome,
                f32 today, f32 tmat, f32 s, f32 t
              ) =
    let {a, b, rho, nu, sigma} = genome   in

    let ts    = date_act_365(t,    s)     in
    let tmatt = date_act_365(tmat, t)     in

    let tmat0 = date_act_365(tmat, today) in
    let tmats = date_act_365(tmat, s)     in
    let t0    = date_act_365(t,    today) in
    let s0    = date_act_365(s,    today) in

    let tmp1  = (sigma*sigma)/(a*a)+(sigma*rho*nu)/(a*b)          in
    let tmp2  = 1.0 - exp32(-a * ts)                              in
    let tmp3  = sigma * sigma / (2.0 * a * a)                     in
    let tmp4  = rho * sigma * nu / (b * (a + b))                  in
    let tmp5  = exp32(-a * tmatt) - exp32(-a * (tmats + ts))      in
    let tmp6  = exp32(-b * tmatt) - exp32(-b*tmat0 - a*t0 + (a+b)*s0)

        in tmp1 * tmp2 - ( tmp3 * tmp5 ) - ( tmp4 * tmp6 )


------------------------------------------------------------------/
-- the first parameter `genome' is the five-genes genome used in
--     the genetic algorithms that models the interest rate
-- the other parameter are times: today, maturity, and the
--      lower and upper bound of the considered time interval
--
-- the result is: y drift term in tmat-forward measure
------------------------------------------------------------------/

fun f32 bigmy( {f32,f32,f32,f32,f32} genome,
                f32 today, f32 tmat, f32 s, f32 t
              ) =
    let {a, b, rho, nu, sigma} = genome   in

    let ts    = date_act_365(t,    s)     in
    let tmatt = date_act_365(tmat, t)     in
    let tmat0 = date_act_365(tmat, today) in
    let tmats = date_act_365(tmat, s)     in
    let t0    = date_act_365(t,    today) in
    let s0    = date_act_365(s,    today) in

    let tmp1  = nu*nu/(b*b)+sigma*rho*nu/(a*b)     in
    let tmp2  = 1.0 - exp32(-b * ts)               in
    let tmp3  = nu * nu / (2.0 * b * b)            in
    let tmp4  = sigma * rho * nu / (a * (a + b))         in
    let tmp5  = exp32(-b * tmatt) - exp32(-b * (tmats + ts)) in
    let tmp6  = exp32(-a * tmatt) - exp32(-a*tmat0 - b*t0 + (a+b)*s0)

        in tmp1 * tmp2 - ( tmp3 * tmp5 ) - ( tmp4 * tmp6 )

------------------------------------------------------------------/
-- the first  parameter `today' is today (and very different from tomorrow)
-- the second parameter is the swaption
-- the third  parameter is the implied volability
--
-- the result is: the swaption's price
------------------------------------------------------------------/

fun f32 black_price(f32 today, {f32,f32,f32} swaption, f32 vol ) =
    let {maturity, swap_sched, {strike,lvl}} =
                        extended_swaption_of_swaption( swaption ) in

    let sqrtt = date_act_365(maturity, today) in

    let d1 = 0.5*vol*sqrtt in
    let d2 = 0.0 - d1      in
        lvl * strike * ( uGaussian_P(d1) - uGaussian_P(d2) )


----------------------------------------------------------/
----------------------------------------------------------/
--/ Main function of Module G2PP: pricer_of_swaption    --/
----------------------------------------------------------/
----------------------------------------------------------/

--------------
-- def a_fun(end_date,a,b,rho,nu,sigma,today,maturity,zc_mat,v0_mat):
--   # Brigo and Mercurio: defined top p. 148
--   v0_end,dummyA,dummyB=bigv(a,b,rho,nu,sigma,tau=Date.act_365(end_date,today))
--   vt_end,ba,bb=bigv(a,b,rho,nu,sigma,tau=Date.act_365(end_date,maturity))
--   res=zc(end_date)/zc_mat*N.exp(0.5*(vt_end-v0_end+v0_mat))
--   return res,ba,bb
--------------

fun f32
pricer_of_swaption( f32                       today,
                    {f32,f32,f32}           swaption,
                    {f32,f32,f32,f32,f32} genome,
                    [f32]                     x_quads,
                    [f32]                     w_quads
                  ) =
    let swaption = extended_swaption_of_swaption(swaption) in
    let {maturity, schedulei, {strike,unused}} = swaption  in

    let n_schedi = size(0, schedulei)                      in
    let ci = map(   fn f32 (int i) =>
                        let {d_beg,d_end} = schedulei[i]   in
                        let tau = date_act_365(d_end,d_beg)in
                        if(i == n_schedi-1)
                        then 1.0 + tau*strike
                        else       tau*strike
                    , iota(n_schedi)
                )                                       in
--
    let tmat0    = date_act_365( maturity, today() )    in
    let {v0_mat, dummyA, dummyB} = bigv( genome, tmat0) in
    let zc_mat   = zc(maturity)                         in
--
    let {a,b,rho,nu,sigma} = genome                     in
    let sigmax = sigma * sqrt32( b_fun(2.0*a, tmat0) )    in
    let sigmay = nu    * sqrt32( b_fun(2.0*b, tmat0) )    in
    let rhoxy  = (rho * sigma * nu) / (sigmax * sigmay)
                    * b_fun(a+b, tmat0)                 in

    let rhoxyc = 1.0 - rhoxy * rhoxy                    in
    let rhoxycs= sqrt32( rhoxyc )                         in
    let t2     = rhoxy / (sigmax*rhoxycs)               in
    let sigmay_rhoxycs = sigmay * rhoxycs               in
    let t4     = (rhoxy * sigmay) / sigmax              in
--
    let mux    = -bigmx( genome, today, maturity, today, maturity ) in
    let muy    = -bigmy( genome, today, maturity, today, maturity ) in
--
    let {scheduleix, scheduleiy} = unzip(schedulei) in
--
    let {bai, bbi, aici, log_aici, t1_cst, scale} = unzip (
            map(fn {f32,f32,f32,f32,f32,f32} ({f32,f32} dc) =>
                    let {end_date, ci} = dc                                   in

                  -- Begin Brigo and Mercurio: defined top p. 148
                    let {v0_end, dummyA, dummyB} =
                            bigv( genome, date_act_365(end_date, today   ) )  in

                    let {vt_end, bai, bbi} =
                            bigv( genome, date_act_365(end_date, maturity) )  in

                    let aa = zc(end_date) / zc_mat *
                                exp32( 0.5 * (vt_end-v0_end+v0_mat) )         in
                  -- END Brigo and Mercurio: defined top p. 148

                    let aici = ci * aa                                        in
                    let log_aici = log32(aici)                                  in

                    let t3 = muy - 0.5*rhoxyc*sigmay*sigmay*bbi               in
                    let cst= bbi * (mux*t4 - t3)                              in
                    let t1_cst = aici * exp32(cst)                            in
                    let scale  = -(bai + bbi*t4)                              in
                        {bai, bbi, aici, log_aici, t1_cst, scale}

                , zip(scheduleiy, ci)
            )
        )                                                               in

    let babaici = zip(bai, bbi, aici, log_aici)                         in
    let scals   = {b, sigmax, sigmay, rhoxy, rhoxyc, rhoxycs, mux, muy} in

    let eps = 0.5 * sigmax                                     in
    let f   = exactYhat( n_schedi, scals, babaici, mux       ) in
    let g   = exactYhat( n_schedi, scals, babaici, mux + eps ) in
    let h   = exactYhat( n_schedi, scals, babaici, mux - eps ) in
    let df  = 0.5 * ( g - h ) / eps  in

    let sqrt2sigmax = sqrt32(2.0) * sigmax                       in

    let tmps = map(
                    fn f32 ( {f32,f32} quad ) =>
                        let {x_quad, w_quad} = quad       in
                        let x = sqrt2sigmax*x_quad + mux  in

                        ------------------------------------------/
                        -- BEGIN function integrand(x) inlined
                        ------------------------------------------/
                        let tmp = (x - mux) / sigmax      in
                        let t1  = exp32( -0.5 * tmp * tmp ) in

                        let yhat_x = f + df*(x - mux)     in
                        let h1  = ( (yhat_x - muy) / sigmay_rhoxycs ) - t2*( x - mux ) in

                        let tmps= map(  fn f32 ( {f32,f32,f32} bbit1cstscale ) =>
                                            let {bbii, t1_csti, scalei} = bbit1cstscale in
                                            let h2 = h1 + bbii * sigmay_rhoxycs in
                                                t1_csti * exp32(scalei*x) * uGaussian_P(-h2)
                                        , zip(bbi, t1_cst, scale)
                                     ) in
                        let accum = reduce(+, 0.0, tmps) in
                        let integrand_res = t1 * ( uGaussian_P(-h1) - accum )
                        ------------------------------------------/
                        -- END   function integrand(x) inlined
                        ------------------------------------------/

                        in w_quad * integrand_res

                  , zip(x_quads, w_quads)
                  )                        in
    let sum = reduce(+, 0.0, tmps)      in
            zc_mat * ( sum / sqrt32( PI() ) )


--------------------------
-- Root finder
--------------------------
fun f32 exactYhat( int n_schedi,
                    {f32,f32,f32,f32,f32,f32,f32,f32} scals,
                    [{f32,f32,f32,f32}] babaicis,
                    f32 x
                  ) =
    -- ugaussian_Pinv(k)=1.0e-4
    let k=-3.71901648545568                                   in


    let uplos = map(  fn {f32,f32} ({f32,f32,f32,f32} babaici) =>
                        let {bai,bbi,aici,log_aici} = babaici in
                        let baix                    = bai * x in
                            {   aici * exp32( -baix ),
                                (log_aici-baix) / bbi
                            }
                      , babaicis
                   )                                          in
    let {ups, los} = unzip(uplos)             in
    let {up,  lo } = reduce( fn {f32,f32} ({f32,f32} x, {f32,f32} y) =>
			       let {a1, b1} = x in 
			       let {a2, b2} = y in
			       {a1 + a2, max(b1, b2) }
			   , {0.0, -infinity()}
			   , uplos 
			   )
--    let up = reduce(+, 0.0, ups)           in
--    let lo = reduce(max, -infinity(), los)    in
    in
    let {bai, bbi, aici, log_aici} = unzip(babaicis) in

    if(n_schedi < 2) -- == 1
    then lo
    else
         let log_s = log32(up)                  in
         let tmp   = log_s / bbi[n_schedi-1]  in
         let up    = if( tmp<= 0.0 ) then tmp
                     else
                       let tmp = log_s/bbi[0] in
                       if(0.0<= tmp) then tmp
                       else -infinity()       in

         let yl = lo - epsilon()              in
         let yu = up + epsilon()              in

         let {b, sigmax, sigmay, rhoxy, rhoxyc, rhoxycs, mux, muy} = scals   in

         -- y01 x = y0, y1 / phi(h_i(x, y0)) <= epsilon, 1 - phi(h_i(x, y1)) <= epsilon
         let y0 = sigmay * (rhoxy*(x-mux)/sigmax+k*rhoxycs) - rhoxyc/b + muy  in
         let y1 = sigmay * (rhoxy*(x-mux)/sigmax-k*rhoxycs) + muy             in

         if      (y1 <= yl) then y1 + 1.0  -- yhat is greater than y1 => 1 - phi(h_i(x, yhat)) < epsilon
         else if (yu <= y0) then y0 - 1.0  -- yhat is lower than y0 => phi(h_i(x, yhat)) < epsilon)
         else
              ---- `scales' is the same as `ups', however, if this branch
              ---- is not oftenly taken, it might make sense to duplicate computation,
              ---- since if the previous `ups' is in a map-reduce pattern!
              --let scales  = map(  fn f32 ( {f32,f32} baiaici) =>
              --                      let {bai,aici} = baiaici in
              --                      aici * exp( -bai * x )
              --                    , zip(bai, aici)
              --                 )        in
	      let scales  = ups         in
              let root_lb = max(yl, y0) in
              let root_ub = min(yu, y1) in
              let {root, iteration, error} =
                    rootFinding_Brent(1, zip(scales, bbi), root_lb, root_ub, 1.0e-4, 1000) in

              if      ( error == -infinity() ) then y0 - 1.0
              else if ( error ==  infinity() ) then y1 + 1.0
              else                                  root

------------------------------------------------------
----   Date: Gregorian calendar
------------------------------------------------------

fun int MOD(int x, int y) = x - (x/y)*y

fun int hours_in_dayI   () = 24
fun int minutes_in_dayI () = hours_in_dayI() * 60
fun int minutes_to_noonI() = (hours_in_dayI() / 2) * 60

fun f32 minutes_in_day  () = 24.0*60.0


--                           year*month*day*hour*mins
fun int date_of_gregorian( {int,int,int,int,int} date) =
    let {year, month, day, hour, mins} = date in
    let ym =
        if(month == 1 || month == 2)
        then    ( 1461 * ( year + 4800 - 1 ) ) / 4 +
                  ( 367 * ( month + 10 ) ) / 12 -
                  ( 3 * ( ( year + 4900 - 1 ) / 100 ) ) / 4
        else    ( 1461 * ( year + 4800 ) ) / 4 +
                  ( 367 * ( month - 2 ) ) / 12 -
                  ( 3 * ( ( year + 4900 ) / 100 ) ) / 4 in
    let tmp = ym + day - 32075 - 2444238

            in tmp * minutes_in_dayI() + hour * 60 + mins


fun {int,int,int,int,int}
gregorian_of_date ( int minutes_since_epoch ) =
    let jul = minutes_since_epoch / minutes_in_dayI() in
    let l = jul + 68569 + 2444238 in
    let n = ( 4 * l ) / 146097 in
    let l = l - ( 146097 * n + 3 ) / 4 in
    let i = ( 4000 * ( l + 1 ) ) / 1461001 in
    let l = l - ( 1461 * i ) / 4 + 31 in
    let j = ( 80 * l ) / 2447 in
    let d = l - ( 2447 * j ) / 80 in
    let l = j / 11 in
    let m = j + 2 - ( 12 * l ) in
    let y = 100 * ( n - 49 ) + i + l in

    --let daytime = minutes_since_epoch mod minutes_in_day in
    let daytime = MOD( minutes_since_epoch, minutes_in_dayI() ) in

    if ( daytime == minutes_to_noonI() )

    --then [year = y; month = m; day = d; hour = 12; minute = 0]
    then {y, m, d, 12, 0}

    --else [year = y; month = m; day = d; hour = daytime / 60; minute = daytime mod 60]
    else {y, m, d, daytime / 60, MOD(daytime, 60) }


fun bool check_date(int year, int month, int day) =
    let tmp1 = ( 1 <= day && 1 <= month && month <= 12 && 1980 <= year && year <= 2299 ) in
    let tmp2 = ( day <= 28 ) in

    let tmp3 = if      ( month == 2 )
               then let tmpmod = MOD(year, 100) in
                        ( day == 29 && MOD(year, 4) == 0 && ( year == 2000 || (! (tmpmod == 0)) ) )
               else if ( month == 4 || month == 6 || month == 9 || month == 11 )
                    then ( day <= 30 )
                    else ( day <= 31 )

        in tmp1 && (tmp2 || tmp3)


fun f32 days_between(f32 t1, f32 t2) =
  (t1 - t2) / minutes_in_day()

fun f32 date_act_365(f32 t1, f32 t2) = days_between(t1, t2) / 365.0

fun bool leap(int y) = ( MOD(y,4) == 0  && ( (!(MOD(y,100)==0)) || (MOD(y,400)==0) ) )

fun int end_of_month(int year, int month) =
    if      ( month == 2 && leap(year) )                           then 29
    else if ( month == 2)                                          then 28
    else if ( month == 4 || month == 6 || month == 9 || month == 11 ) then 30
    else                                                               31


fun f32 add_months ( f32 date, f32 rnbmonths ) =
    let nbmonths          = int(rnbmonths)                 in
    let {y, m, d, h, min} = gregorian_of_date( int(date) ) in
    let m = m + nbmonths                                     in
    let {y, m} = {y + (m-1) / 12, MOD(m-1, 12) + 1}          in
    let {y, m} = if (m <= 0) then {y - 1, m + 12} else {y, m} in
    let resmin = date_of_gregorian ( {y, m, minI( d, end_of_month(y, m) ), 12, 0} ) in

            f32(resmin)


fun f32 add_years(f32 date, f32 nbyears) =
        add_months(date, nbyears * 12.0)


--assert max_date==168307199, a.k.a. "2299-12-31T23:59:59"
fun f32 max_date () = 168307199.0

--assert min_date==3600, a.k.a., "1980-01-01T12:00:00"
fun f32 min_date () = 3600.0

-- Date.of_string("2012-01-01")
fun f32 today    () = f32( date_of_gregorian( {2012, 1, 1, 12, 0} ) )

-- Interest rate calibration
-- ==
-- tags { notravis nobench no_opencl }
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
fun main(pop:  int, mcmc_conv: int, l: int
        , swaptions: [][]f32,     ll: int
        , hermCoefs: []f32
        , hermWeights: []f32,   lll: int
        , sobDirVct: []int
        ): (f32,f32,f32,f32,f32,f32,[][]f32) =
  let hermData  = zip hermCoefs hermWeights in
  interestCalibKernel(pop, mcmc_conv, swaptions, hermData, sobDirVct)

----------------------------------------------------
--/ Prepares the output summary for each swaption
--/ [calibration price, black price, % error]
----------------------------------------------------
fun makeSummary(quote_prices: [](f32,f32)): [][]f32 =
  map (\(qp: (f32,f32)): []f32  ->
         let (black_price, calib_price) = qp
         let err_ratio =  (calib_price - black_price) / black_price in
         [10000.0*calib_price, 10000.0*black_price, 100.0*fabs(err_ratio)]
     ) (quote_prices
     )

----------------------------------------------------
--/ COMPUTATIONAL KERNEL
----------------------------------------------------
fun interestCalibKernel(pop:  int
                       , mcmc_conv: int
                       , swaptions: [][]f32
                       , hermdata: [](f32,f32)
                       , sobDirVct: []int
                       ): (f32,f32,f32,f32,f32,f32,[][]f32) =
  -- initialize the genomes
  let genomes = map  (\(i: int): []f32  ->
                        let k   = 5*i + 1
                        let z5s = map  (+k) (iota(5) )
                        let sobs= map  (sobolInd(sobDirVct)) z5s
                        in  initGenome( copy(sobs) )
                    ) (iota(pop) )
  -- evaluate genomes
  let logLiks = map  (\(genome: []f32): f32  ->
                        let qtprs = map  (evalGenomeOnSwap(genome,hermdata)
                                        ) swaptions
                        let terms = map  logLikelihood qtprs in
                                  reduce (+) (0.0) terms
                    ) genomes
--  logLiks
--  genomes[pop/2]
  let proposals = copy(genomes)
  let sob_offs = 5*pop+1        in
  loop ((genomes,proposals,logLiks,sob_offs)) =
    for j < mcmc_conv do
      let rand01   = sobolInd sobDirVct sob_offs
      let move_type= selectMoveType(rand01)
      let sob_offs = sob_offs + 1

      let (proposals, fb_rats, sob_offs) =
        if (move_type == 1) --  move_type == DIMS_ALL
        then let sob_mat =
                 map (\(i: int): []f32  ->
                        let k   = 5*i + sob_offs
                        let z5s = map (+k) (iota(5) ) in
                        map  (sobolInd(sobDirVct)) z5s
                    ) (iota(pop) )
             let new_gene_rat =
                 map mutate_dims_all (zip (sob_mat) genomes proposals )
             let (new_genomes, fb_rats) = unzip(new_gene_rat)
             in  (new_genomes, fb_rats, sob_offs+5*pop)

        else
        if (move_type == 2) -- move_type == DIMS_ONE
        then let s1  = sobolInd sobDirVct sob_offs
             let dim_j = int( s1 * f32(5) )
             let sob_mat =
                 map (\(i: int): []f32  ->
                        let k   = 5*i + sob_offs + 1
                        let z5s = map (+k) (iota(5)) in
                        map  (sobolInd(sobDirVct)) z5s
                    ) (iota(pop) )

             let new_gene_rat =
                 map (mutate_dims_one(dim_j)) (zip (sob_mat) genomes proposals )
             let (new_genomes, fb_rats) = unzip(new_gene_rat)
             in  (new_genomes, fb_rats, sob_offs+5*pop+1)

        else                -- move_type == DEMCMC
             let new_genomes =
                 map (\(i: int): *[]f32  ->
                        let kk  = 8*i + sob_offs
                        let s1  = sobolInd sobDirVct kk
                        let k = int( s1 * f32(pop-1) )  -- random in [0,pop-1)
                        let (k,cand_UB) = if k == i
                                          then (pop-1, pop-2)
                                          else (k,     pop-1)

                        let s2  = sobolInd sobDirVct (kk+1)
                        let l = int( s2*f32(cand_UB) ) -- random in [0,cand_UB -1)
                        let l = if (l == i) || (l == k)
                                then cand_UB
                                else l

                        let s3      = sobolInd sobDirVct (kk+2)
                        let z5s     = map (+(kk+3)) (iota(5) )
                        let sob_row = map (sobolInd(sobDirVct)) z5s in
                            mcmc_DE(s3, sob_row, genomes[i], genomes[k], genomes[l])
                    ) (iota(pop) )
             in  (new_genomes, replicate pop 1.0, sob_offs+8*pop)

      let new_logLiks =
          map  (\(genome: []f32): f32  ->
                    let qtprs = map  (evalGenomeOnSwap(genome,hermdata)
                                    ) swaptions
                    let terms = map  logLikelihood qtprs in
                    reduce (+) (0.0) terms
              ) proposals
      let res_gene_liks =
          map (\(tup: ([]f32,f32,[]f32,f32,f32,int)): (*[]f32,f32)  ->
                 let (gene, logLik, new_gene, new_logLik, fb_rat, i) = tup
                 let acceptance = min( 1.0, exp32(new_logLik - logLik)*fb_rat )
                 let rand01     = sobolInd sobDirVct (sob_offs+i)
                 let (res_gene, res_logLik) =
                       if ( rand01 < acceptance )
                       then (new_gene, new_logLik)
                       else (gene,     logLik    )
                 in (copy(res_gene), res_logLik)
             ) (zip genomes logLiks proposals (new_logLiks) (fb_rats) (iota(pop))
             )

      let (res_genomes, res_logLiks) = unzip(res_gene_liks) in
      (res_genomes, proposals, res_logLiks, sob_offs+pop)
  -- END OF DO LOOP!!!


  let (winner_ind, winner_logLik) =
      reduce (\(t1: (int,f32)) (t2: (int,f32)): (int,f32)  ->
                let (i1, v1) = t1 let (i2, v2) = t2 in
                if (v1 < v2) then (i2, v2) else (i1, v1)
            ) (0, -infinity()) (zip (iota(pop)) logLiks
            )

  let winner = genomes[winner_ind]
  let winner_quote_prices =
      map  (evalGenomeOnSwap(genomes[winner_ind],hermdata)
          ) swaptions
  in  ( winner[0],winner[1],winner[4],winner[3],winner[2],winner_logLik
      , makeSummary(winner_quote_prices)
      )

--------------------------------------------------/
--/ Constants and Utility Functions
--------------------------------------------------/
fun eps0   (): f32 = 1.0e-3
fun eps    (): f32 = 1.0e-5
fun pi     (): f32 = 3.1415926535897932384626433832795

fun is_cauchy_llhood  (): bool = true
fun llhood_cauchy_offs(): f32 = 5.0
fun llhood_normal_offs(): f32 = 1.0

fun r       (): f32 = 0.03
fun infinity(): f32 = 1.0e49
fun epsilon (): f32 = eps()

fun itMax   (): int = 10000


fun min(a: f32, b: f32): f32 = if(a < b) then a else b
fun max(a: f32, b: f32): f32 = if(a < b) then b else a

fun minI(a: int, b: int): int = if(a < b) then a else b
fun maxI(a: int, b: int): int = if(a < b) then b else a

fun fabs(a: f32): f32 = if(a < 0.0) then -a else a

fun equal(x1: f32, x2: f32): bool =
    fabs(x1-x2) <= 1.0e-8

--------------------------------------------------/
--/ evaluating a genome on a swaption and getting
--/ back the quote and the estimated price
--------------------------------------------------/
fun evalGenomeOnSwap (genomea: []f32,
                      hermdata: [](f32,f32))
                     (swaption: []f32): (f32,f32) =
  let (a,b,rho,nu,sigma) = (genomea[0],genomea[1],genomea[2],genomea[3],genomea[4])
  let swap_freq  = swaption[1]
  let maturity   = add_years( today(), swaption[0] )
  let n_schedi   = int(12.0 * swaption[2] / swap_freq)

  let tmat0      = date_act_365( maturity, today() )

  ----------------------------------------
  -- Quote (black) price computation
  -- This computation does not depend on
  --   the swaption and can be separated
  --   and hoisted outside the swaption
  --   and convergence loop ...
  ----------------------------------------
  let a12s = map  (\(i: int): (f32,f32,f32)  ->
                     let a1 = add_months( maturity, swap_freq*f32(i) )
                     let a2 = add_months( a1, swap_freq ) in
                     ( zc(a2) * date_act_365(a2, a1), a1, a2 )
                 ) (iota(n_schedi) )
  let (lvl, t0, tn) = reduce (\(tup1:  (f32,f32,f32))
                                 (tup2: (f32,f32,f32) ): (f32,f32,f32)  ->
                                let (lvl,t0,tn) = tup1
                                let (a12,a1,a2) = tup2 in
                                ( lvl + a12, min(t0,a1), max(tn,a2) )
                            ) (0.0, max_date(), min_date()) a12s

  let strike     = ( zc(t0) - zc(tn) ) / lvl
  let d1         = 0.5 * swaption[3] * tmat0
  let new_quote  = lvl * strike * ( uGaussian_P(d1) - uGaussian_P(-d1) )

  -- starting new price compuation
  let (v0_mat, dummy1, dummy2) = bigv( (a,b,rho,nu,sigma), tmat0 )
  let mux = 0.0 - bigmx( (a,b,rho,nu,sigma), today(), maturity, today(), maturity )
  let muy = 0.0 - bigmy( (a,b,rho,nu,sigma), today(), maturity, today(), maturity )
  let zc_mat = zc(maturity)
  let sqrt_bfun_a = sqrt32( b_fun(2.0*a, tmat0) )
  let sqrt_bfun_b = sqrt32( b_fun(2.0*b, tmat0) )
  let rhoxy  = rho * b_fun(a+b, tmat0) / (sqrt_bfun_a * sqrt_bfun_b)
  let sigmax = sigma * sqrt_bfun_a
  let sigmay = nu    * sqrt_bfun_b
  --
  let rhoxyc = 1.0 - rhoxy * rhoxy  -- used in reduction kernel
  let rhoxycs= sqrt32( rhoxyc )       -- used in reduction kernel
  let sigmay_rhoxycs = sigmay * rhoxycs
  let t4     = (rhoxy * sigmay) / sigmax

  -- computing n_schedi-size temporary arrays
  let tmp_arrs =
          map (\(i: int): (f32,f32,f32,f32,f32,f32,f32)  ->
                 let beg_date = add_months( maturity, swap_freq*f32(i) )
                 let end_date = add_months( beg_date, swap_freq  )
                 let res      = date_act_365( end_date, beg_date ) * strike
                 let cii      = if i==(n_schedi-1) then 1.0 + res  else res

                 let date_tod1= date_act_365(end_date, today())
                 let (v0_end,dummy1,dummy2) = bigv( (a,b,rho,nu,sigma), date_tod1 )

                 let date_tod2= date_act_365(end_date, maturity)
                 let (vt_end, baii, bbii  ) = bigv( (a,b,rho,nu,sigma), date_tod2 )

                 let expo_aici = 0.5 * (vt_end - v0_end + v0_mat)
                 let fact_aici = cii * zc(end_date) / zc_mat      in

                     ( baii
                     , bbii
                     , fact_aici * exp32( expo_aici )
                     , log32( fact_aici ) + expo_aici
                     , 0.0  - ( baii + bbii * t4 )
                     , fact_aici
                     , bbii * (mux * t4 - (muy - 0.5*rhoxyc*sigmay*sigmay*bbii) ) + expo_aici
                     )
             ) (iota(n_schedi) )
  let (bas, bbs, aicis, log_aicis, scales, cs, t1_cs) = unzip(tmp_arrs)
  let scals = (b, sigmax, sigmay, rhoxy, rhoxyc, rhoxycs, mux, muy)
  let exact_arrs = zip bas bbs aicis (log_aicis )

  -- exactYhat via Brent method
  let eps = 0.5 * sigmax
  let f   = exactYhat( n_schedi, scals, exact_arrs, mux       )
  let g   = exactYhat( n_schedi, scals, exact_arrs, mux + eps )
  let h   = exactYhat( n_schedi, scals, exact_arrs, mux - eps )

  -- integration with Hermite polynomials
  let herm_arrs   = zip bbs scales cs (t1_cs )
  let df          = 0.5 * ( g - h ) / eps
  let sqrt2sigmax = sqrt32(2.0) * sigmax
  let t2          = rhoxy / (sigmax*rhoxycs)

  let accums = map (\(herm_el: (f32,f32)): f32  ->
                      let (x_quad, w_quad) = herm_el
                      let x  = sqrt2sigmax * x_quad + mux
                      let yhat_x = f + df*(x - mux)
                      let h1 = ( (yhat_x - muy) / sigmay_rhoxycs ) - t2*( x - mux )
                      let accum1s = map (\(tup: (f32,f32,f32,f32)): f32  ->
                                           let (bbi, scalei, csi, t1_csi) = tup
                                           let h2 = h1 + bbi * sigmay_rhoxycs
                                           let expo_aici = t1_csi + scalei*x
                                           let fact_aici = csi
                                           let expo_part = uGaussian_P_withExpFactor( -h2, expo_aici )
                                           in  fact_aici * expo_part
                                       ) (zip bbs scales cs (t1_cs )
                                       )
                      let accum1 = reduce (+) (0.0) accum1s
                      let tmp    = sqrt32(2.0) * x_quad
                      let t1     = exp32( - 0.5 * tmp * tmp )     in
                      w_quad * t1 * ( uGaussian_P(-h1) - accum1 )
                  ) hermdata

  let accum = reduce (+) (0.0) accums
  let new_price = zc_mat * ( accum / sqrt32( pi() ) ) in
  (new_quote, new_price)


--------------------------------------------------/
--/ Sobol Random Number Generation
--------------------------------------------------/
--fun []f32 getChunkSobNums(int sob_ini, int CHUNK, []int dirVct) =
--  let norm_fact = 1.0 / ( f32(1 << size(0,dirVct)) + 1.0 ) in
--  let sob_inds  = map(+sob_ini, iota(CHUNK))
--  in  map( sobolInd(dirVct,norm_fact), sob_inds )

fun sobolInd(dirVct:  [m]int) (n: int): f32 =
    -- placed norm_fact here to check that hoisting does its job!
    let norm_fact = 1.0 / ( f32(1 << m) + 1.0 )
    let n_gray = (n >> 1) ^ n
    let res = 0 in
    loop (res) =
      for i < m do
        let t = 1 << i in
        if (n_gray & t) == t
        then res ^ dirVct[i]
        else res
    in  f32(res) * norm_fact

--------------------------------------------------/
--/ Genome Implementation
--------------------------------------------------/
fun genomeBounds(): [](f32,f32,f32) =
    [ (eps0(),     1.0-eps0(), 0.02)
    , (eps0(),     1.0-eps0(), 0.02)
    , (eps0()-1.0, 1.0-eps0(), 0.0 )
    , (eps0(),     0.2,        0.01)
    , (eps0(),     0.2,        0.04)
    ]

fun initGenome (rand_nums: []f32): []f32 =
  map  (\(tup: (f32, (f32,f32,f32))): f32  ->
                    let (r01, (g_min, g_max, g_ini)) = tup
                    in  r01*(g_max - g_min) + g_min

               ) (zip (rand_nums) (genomeBounds())
               )

fun selectMoveType(r01: f32): int =
  if r01 <= 0.2   then 1 -- r01 in [0.0, 0.2] -> DIMS_ALL
  else
    if r01 <= 0.5 then 2 -- r01 in (0.2, 0.5] -> DIMS_ONE
    else               3 -- r01 in (0.5, 1.0) -> DEMCMC
--( MV_EL_TYPE(0.2,DIMS_ALL), MV_EL_TYPE(0.5,DIMS_ONE), MV_EL_TYPE(1.0,DEMCMC) );

fun moves_unif_ampl_ratio(): f32 = 0.005

fun mutate_dims_all(tup: ([n]f32,[]f32,[]f32)): (*[]f32,f32) =
  let (sob_row, orig, muta) = tup
  let gene_bds = genomeBounds()
  let amplitude = moves_unif_ampl_ratio()
  let gene_rats = map mutateHelper (
                       zip (replicate n amplitude) (sob_row) orig muta (gene_bds) )
  let (tmp_genome, fb_rats) = unzip(gene_rats)
  let new_genome= map constrainDim (zip (tmp_genome) (gene_bds) )
  let fb_rat    = reduce (*) (1.0) (fb_rats)
  in  (copy(new_genome), fb_rat)

fun mutate_dims_one(dim_j: int) (tup: ([]f32,[n]f32,[]f32)): (*[]f32,f32) =
  let (sob_row, orig, muta) = tup
  let gene_bds = genomeBounds()
  let amplitudes= map (\(i: int): f32  ->
                          if i == dim_j then moves_unif_ampl_ratio() else 0.0
                     ) (iota(n) )

  let gene_rats = map mutateHelper (zip amplitudes (sob_row) orig muta (gene_bds) )
  let (tmp_genome, fb_rats) = unzip(gene_rats)
  let new_genome= map constrainDim (zip (tmp_genome) (gene_bds) )
  let fb_rat    = reduce (*) (1.0) (fb_rats)
  in  (copy(new_genome), fb_rat)


fun mcmc_DE(r01: f32, sob_row: []f32, g_i: []f32, g_k: []f32, g_l: []f32): *[]f32 =
  let gene_bds = genomeBounds()
  let gamma_avg = 2.38 / sqrt32(2.0*5.0)
  let ampl_ratio= 0.1 * moves_unif_ampl_ratio()
  let gamma1    = gamma_avg - 0.5 + r01
  let mm_diffs  = map (\(tup: (f32,f32,f32)): f32  ->
                           let (g_min, g_max, uu) = tup
                           in  g_max - g_min
                     ) (gene_bds )

  let tmp_genome = map (perturbation(gamma1,ampl_ratio))
                   (zip (g_i) (g_k) (g_l) (sob_row) (mm_diffs))

  in  copy( map constrainDim (zip (tmp_genome) (gene_bds) ) )


fun perturbation(gamma1:  f32, ampl_rat : f32)
                (gene: f32, gene_k: f32, gene_l: f32,   r01: f32, mm_diff: f32 ): f32 =
  let amplitude     = fabs( mm_diff * ampl_rat )
  let semiamplitude = amplitude / 2.0
  let perturb       = ( amplitude * r01 - semiamplitude )
  in  gene + perturb + gamma1 * ( gene_k - gene_l )

fun mutateHelper(ampl_ratio:  f32, r01: f32, gene: f32, prop: f32, gmm: (f32,f32,f32)): (f32,f32) =
  let (g_min, g_max, g_ini) = gmm
  let amplitude     = fabs( (g_max - g_min) * ampl_ratio )
  let semiamplitude = amplitude / 2.0

  let tmp_min_max = min( g_max, gene + semiamplitude )
  let tmp_max_min = max( g_min, gene - semiamplitude )
  let forward_range = tmp_min_max - tmp_max_min

  let tmp_min_max = min( g_max, prop + semiamplitude )
  let tmp_max_min = max( g_min, prop - semiamplitude )
  let backward_range = tmp_min_max - tmp_max_min

  let bf_fact = if 0.0 < semiamplitude
                then (backward_range / forward_range)
                else 1.0
  in (gene + amplitude * r01 - semiamplitude, bf_fact)


fun constrainDim(gene:  f32, tup: (f32,f32,f32) ): f32 =
  let (g_min, g_max, g_ini) = tup
  in  max( g_min, min(g_max, gene) )

--------------------------------------------------------------/
--/ Likelihood implementation
--------------------------------------------------------------/
fun logLikelihood(y_ref: f32, y: f32): f32 =
  if   is_cauchy_llhood()
  then logLikeCauchy(y_ref, y)
  else logLikeNormal(y_ref, y)

fun normalPdf(z:  f32, mu: f32, sigma: f32 ): f32 =
    let sigma  = fabs(sigma)
    let res    = 1.0 / (sigma * sqrt32(2.0*pi()))
    let ecf    = (z-mu) * (z-mu) / (2.0 * sigma * sigma) in
    res * exp32( 0.0 - ecf )
fun logLikeNormal(y_ref:  f32, y: f32): f32 =
    let sigma = (y_ref / 50.0) * llhood_normal_offs()
    let pdfs  = normalPdf( y, y_ref, sigma ) in
    log32(pdfs + 1.0e-20)

fun cauchyPdf(z:  f32, mu: f32, gamma: f32 ): f32 = -- mu=0.0, gamma=4.0
    let x = (z-mu) / gamma in
    1.0 / ( pi() * gamma * (1.0 + x*x) )
fun logLikeCauchy(y_ref:  f32, y: f32 ): f32 =
    let gamma = ( fabs(y_ref) / 50.0 ) * llhood_cauchy_offs() + 0.01
    let pdfs  = cauchyPdf( y, y_ref, gamma ) in
    log32(pdfs + 1.0e-20)

----------------------------------------------------------------
----/ MATH MODULE
----------------------------------------------------------------

---------------------------------------------------------------------------
-- Cumulative Distribution Function for a standard normal distribution

fun uGaussian_P(x: f32): f32 =
    let u = x / sqrt32(2.0)
    let e = if (u < 0.0) then -erf(-u)
                         else  erf( u)
    in 0.5 * (1.0 + e)

fun uGaussian_P_withExpFactor(x:  f32, exp_factor: f32 ): f32 =
    let u   = fabs( x / sqrt32(2.0) )
    let e   = erff_poly_only(u)
    let res = 0.5 * e * exp32(exp_factor-u*u) in
    if ( 0.0 <= x )
    then exp32(exp_factor) - res
    else res

---------------------------------------------------------------------------
-- polynomial expansion of the erf() function, with error<=1.5e-7
--   formula 7.1.26 (page 300), Handbook of Mathematical Functions, Abramowitz and Stegun
--   http:--people.math.sfu.ca/~cbm/aands/frameindex.htm

fun erf(x: f32): f32 =
    let p = 0.3275911
    let (a1,a2,a3,a4,a5) =
        (0.254829592, -0.284496736, 1.421413741, -1.453152027, 1.061405429)
    let t  = 1.0/(1.0+p*x)
    let t2 = t  * t
    let t3 = t  * t2
    let t4 = t2 * t2
    let t5 = t2 * t3        in
         1.0 - (a1*t + a2*t2 + a3*t3 + a4*t4 + a5*t5) * exp32(-(x*x))

---------------------------------------------------------------------------
-- iteration_max = 10000 (hardcoded)

fun erff_poly_only(x:  f32 ): f32 =
    let p = 0.3275911
    let (a1,a2,a3,a4,a5) =
        (0.254829592, -0.284496736, 1.421413741, -1.453152027, 1.061405429)

    let t  = 1.0/(1.0+p*x)
    let t2 = t  * t
    let t3 = t  * t2
    let t4 = t2 * t2
    let t5 = t2 * t3       in
    (a1*t + a2*t2 + a3*t3 + a4*t4 + a5*t5)

------------------------------------------------------------------------
-- if fid = 33 then: to_solve(f32 x) = (x+3.0)*(x-1.0)*(x-1.0)
-- otherwise follows the f32 implementation
------------------------------------------------------------------------

fun to_solve(fid: int, scalesbbi: [](f32,f32), yhat: f32): f32 =
    if(fid == 33) then (yhat+3.0)*(yhat-1.0)*(yhat-1.0)
    else
        let tmps = map (\(scalesbbi:  (f32,f32) ): f32  ->
                            let (scales, bbi) = scalesbbi in
                                scales * exp32(-bbi*yhat)
                        ) scalesbbi
            in reduce (+) (0.0) tmps - 1.0

--------------------------------------------------------/
---- the function-parameter to rootFinding_Brent
--------------------------------------------------------/


fun rootFinding_Brent(fid: int, scalesbbi: [](f32,f32), lb: f32, ub: f32, tol: f32, iter_max: int): (f32,int,f32) =
    let tol      = if(tol     <= 0.0) then 1.0e-9 else tol
    let iter_max = if(iter_max<= 0  ) then 10000  else iter_max
    let (a,b)    = (lb,ub)

    -- `to_solve' is curried so it will need extra arguments!!!
    -- said fid refers to the version of `to_solve', e.g., testing or f32 implem.
    -- Was previous (eval(a),eval(b)), now:
    let (fa,fb)  = ( to_solve(fid, scalesbbi, a), to_solve(fid, scalesbbi, b) ) in

    if(0.0 <= fa*fb)
    then
        if(0.0 <= a) then ( 0.0, 0,  infinity() )  -- root not bracketed above
                     else ( 0.0, 0, -infinity() )  -- root not bracketed below
    else
    let (fa, fb, a, b) = if( fabs(fa) < fabs(fb) )
                         then (fb, fa, b, a)
                         else (fa, fb, a, b)
    let (c,fc)    = (a, fa)
    let mflag     = true
    let it        = 0
    let d         = 0.0     in

    loop ((a,b,c,d,fa,fb,fc,mflag,it)) =
        for i < iter_max do

            if( fb==0.0 || fabs(b-a)<tol )
            then (a,b,c,d,fa,fb,fc,mflag,it)
            else

                -- the rest of the code implements the else branch!

                let s = if( fa==fc || fb == fc )
                        then    b-fb*(b-a)/(fb-fa)

                        else let s1 = (a*fb*fc)/( (fa-fb)*(fa-fc) )
                             let s2 = (b*fa*fc)/( (fb-fa)*(fb-fc) )
                             let s3 = (c*fa*fb)/( (fc-fa)*(fc-fb) ) in
                                s1 + s2 + s3


                let (mflag, s) = if ( ( !((3.0*a+b)/4.0 <= s && s  <= b)    ) ||
                                      (     mflag && fabs(b-c)/2.0 <= fabs(s-b) ) ||
                                      ( !mflag && fabs(c-d)/2.0    <= fabs(s-b) ) ||
                                      (     mflag && fabs(b-c)     <= fabs(tol) ) ||
                                      ( !mflag && fabs(c-d)        <= fabs(tol) )
                                    )
                                 then (true,  (a+b)/2.0)
                                 else (false, s        )

                -- Was previous `eval(s)', Now:
                let fs = to_solve(fid, scalesbbi, s)

                -- d is assigned for the first time here:
                -- it's not used above because mflag is set
                let d = c let (c,fc) = (b, fb)
                let (a,b,fa,fb) = if( fa*fs < 0.0 )
                                  then (a,s,fa,fs)
                                  else (s,b,fs,fb)

                let (a,b,fa,fb) = if( fabs(fa) < fabs(fb) )
                                  then (b,a,fb,fa) -- swap args
                                  else (a,b,fa,fb)

                -- reporting non-convergence!
                let dummy =
                    if(i == iter_max-1)
                    then -- # ERROR: Brent method not converged
                         0
                    else 0

                in (a,b,c,d,fa,fb,fc,mflag,i)

    -- Finally, the result of function rootFinding_Brent is:
    in ( b, it, fb )


----------------------------------------------------------------
----/ G2PP Module
----------------------------------------------------------------

fun zc(t: f32): f32 = exp32(-r() * date_act_365(t, today()))

------------------------------------------/
---- the first param `swaption' is a triple of f32s,
----    i.e., the swaption maturity date, frequency of payment and
----          swapt-term in years (how often to vary the condition of the contract)
----  the result is also a triple:
----    the maturity time stamp,
----    the range of time stamps of for each swap term : [](f32,f32)
----    the strike price
------------------------------------------/
-- Quote (block) price computation
------------------------------------------/
fun extended_swaption_of_swaption(swaption: (f32,f32,f32)): (f32,[](f32,f32),(f32,f32))  =  -- swaption = (sw_mat, freq, sw_ty)
    let (sw_mat, freq, sw_ty) = swaption
    let maturity   = add_years( today(), sw_mat )
    let nschedule  = int(12.0 * sw_ty / freq)

    let a12s = map  (\(i: int): (f32,f32,f32)  ->
                     let a1 = add_months( maturity, freq*f32(i) )
                     let a2 = add_months( a1, freq ) in
                     ( zc(a2) * date_act_365(a2, a1), a1, a2 )
                 ) (iota(nschedule) )

    let (lvl, t0, tn) = reduce (\(tup1:  (f32,f32,f32))
                                   (tup2: (f32,f32,f32)): (f32,f32,f32)  ->
                                let (lvl,t0,tn) = tup1
                                let (a12,a1,a2) = tup2 in
                                ( lvl + a12, min(t0,a1), max(tn,a2) )
                            ) (0.0, max_date(), min_date()) a12s

    let (lvls, a1s, a2s) = unzip( a12s )
    let swap_sched       = zip   a1s a2s
    let strike     = (zc(t0) - zc(tn)) / lvl

    in (maturity, swap_sched, (strike,lvl))


fun b_fun(z: f32, tau: f32): f32 = (1.0-exp32(-z*tau))/z

fun t_fun(sigma: f32, x: f32, tau: f32): f32 =
    let expxtau  = exp32(-x*tau)
    let exp2xtau = expxtau*expxtau in
        sigma*sigma/(x*x)*(tau+2.0/x*expxtau-1.0/(2.0*x)*exp2xtau-3.0/(2.0*x))


------------------------------------------------------------------/
-- the first parameter `genome' is the five-genes genome used in
--     the genetic algorithms that models the interest rate
-- the second parameter is the time
-- the result is V in Brigo and Mercurio's book page 148.
--     \var(\int_t^T [x(u)+y(u)]du)
------------------------------------------------------------------/
fun bigv(genome: (f32,f32,f32,f32,f32), tau: f32): (f32,f32,f32) =
    let (g_a, g_b, g_rho, g_nu, g_sigma) = genome

    -- sanity check; this check should be hoisted higher up
    let g_sigma = if(g_sigma == 0.0) then 1.0e-10 else g_sigma

    let ba = b_fun(g_a,        tau)
    let bb = b_fun(g_b,        tau)
    let t1 = t_fun(g_sigma,g_a,tau)
    let t2 = t_fun(g_nu,   g_b,tau)

    let t3 = 2.0 * g_rho * g_nu * g_sigma / (g_a * g_b)*
             ( tau - ba - bb + b_fun(g_a+g_b, tau) )

        in (t1+t2+t3, ba, bb)

------------------------------------------------------------------/
-- the first parameter `genome' is the five-genes genome used in
--     the genetic algorithms that models the interest rate
-- the other parameter are times: today, maturity, and the
--      lower and upper bound of the considered time interval
--
-- the result is: x drift term in tmat-forward measure
------------------------------------------------------------------/

fun bigmx(genome:  (f32,f32,f32,f32,f32),
                today: f32, tmat: f32, s: f32, t: f32
              ): f32 =
    let (a, b, rho, nu, sigma) = genome

    let ts    = date_act_365(t,    s)
    let tmatt = date_act_365(tmat, t)

    let tmat0 = date_act_365(tmat, today)
    let tmats = date_act_365(tmat, s)
    let t0    = date_act_365(t,    today)
    let s0    = date_act_365(s,    today)

    let tmp1  = (sigma*sigma)/(a*a)+(sigma*rho*nu)/(a*b)
    let tmp2  = 1.0 - exp32(-a * ts)
    let tmp3  = sigma * sigma / (2.0 * a * a)
    let tmp4  = rho * sigma * nu / (b * (a + b))
    let tmp5  = exp32(-a * tmatt) - exp32(-a * (tmats + ts))
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

fun bigmy(genome:  (f32,f32,f32,f32,f32),
                today: f32, tmat: f32, s: f32, t: f32
              ): f32 =
    let (a, b, rho, nu, sigma) = genome

    let ts    = date_act_365(t,    s)
    let tmatt = date_act_365(tmat, t)
    let tmat0 = date_act_365(tmat, today)
    let tmats = date_act_365(tmat, s)
    let t0    = date_act_365(t,    today)
    let s0    = date_act_365(s,    today)

    let tmp1  = nu*nu/(b*b)+sigma*rho*nu/(a*b)
    let tmp2  = 1.0 - exp32(-b * ts)
    let tmp3  = nu * nu / (2.0 * b * b)
    let tmp4  = sigma * rho * nu / (a * (a + b))
    let tmp5  = exp32(-b * tmatt) - exp32(-b * (tmats + ts))
    let tmp6  = exp32(-a * tmatt) - exp32(-a*tmat0 - b*t0 + (a+b)*s0)

        in tmp1 * tmp2 - ( tmp3 * tmp5 ) - ( tmp4 * tmp6 )

------------------------------------------------------------------/
-- the first  parameter `today' is today (and very different from tomorrow)
-- the second parameter is the swaption
-- the third  parameter is the implied volability
--
-- the result is: the swaption's price
------------------------------------------------------------------/

fun black_price(today: f32, swaption: (f32,f32,f32), vol: f32 ): f32 =
    let (maturity, swap_sched, (strike,lvl)) =
                        extended_swaption_of_swaption( swaption )

    let sqrtt = date_act_365(maturity, today)

    let d1 = 0.5*vol*sqrtt
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

fun pricer_of_swaption(today:  f32,
                       swaption: (f32,f32,f32),
                       genome: (f32,f32,f32,f32,f32),
                       x_quads: []f32,
                       w_quads: []f32
                      ): f32 =
    let swaption = extended_swaption_of_swaption(swaption)
    let (maturity, schedulei, (strike,unused)) = swaption

    let n_schedi = (shape schedulei)[0]
    let ci = map (\(i: int): f32  ->
                        let (d_beg,d_end) = schedulei[i]
                        let tau = date_act_365(d_end,d_beg)in
                        if(i == n_schedi-1)
                        then 1.0 + tau*strike
                        else       tau*strike
                    ) (iota(n_schedi)
                )
--
    let tmat0    = date_act_365( maturity, today )
    let (v0_mat, dummyA, dummyB) = bigv( genome, tmat0)
    let zc_mat   = zc(maturity)
--
    let (a,b,rho,nu,sigma) = genome
    let sigmax = sigma * sqrt32( b_fun(2.0*a, tmat0) )
    let sigmay = nu    * sqrt32( b_fun(2.0*b, tmat0) )
    let rhoxy  = (rho * sigma * nu) / (sigmax * sigmay)
                    * b_fun(a+b, tmat0)

    let rhoxyc = 1.0 - rhoxy * rhoxy
    let rhoxycs= sqrt32( rhoxyc )
    let t2     = rhoxy / (sigmax*rhoxycs)
    let sigmay_rhoxycs = sigmay * rhoxycs
    let t4     = (rhoxy * sigmay) / sigmax
--
    let mux    = -bigmx( genome, today, maturity, today, maturity )
    let muy    = -bigmy( genome, today, maturity, today, maturity )
--
    let (scheduleix, scheduleiy) = unzip(schedulei)
--
    let (bai, bbi, aici, log_aici, t1_cst, scale) = unzip (
            map (\(dc: (f32,f32)): (f32,f32,f32,f32,f32,f32)  ->
                    let (end_date, ci) = dc

                  -- Begin Brigo and Mercurio: defined top p. 148
                    let (v0_end, dummyA, dummyB) =
                            bigv( genome, date_act_365(end_date, today   ) )

                    let (vt_end, bai, bbi) =
                            bigv( genome, date_act_365(end_date, maturity) )

                    let aa = zc(end_date) / zc_mat *
                                exp32( 0.5 * (vt_end-v0_end+v0_mat) )
                  -- END Brigo and Mercurio: defined top p. 148

                    let aici = ci * aa
                    let log_aici = log32(aici)

                    let t3 = muy - 0.5*rhoxyc*sigmay*sigmay*bbi
                    let cst= bbi * (mux*t4 - t3)
                    let t1_cst = aici * exp32(cst)
                    let scale  = -(bai + bbi*t4)                              in
                        (bai, bbi, aici, log_aici, t1_cst, scale)

                ) (zip scheduleiy ci
            )
        )

    let babaici = zip bai bbi aici (log_aici)
    let scals   = (b, sigmax, sigmay, rhoxy, rhoxyc, rhoxycs, mux, muy)

    let eps = 0.5 * sigmax
    let f   = exactYhat( n_schedi, scals, babaici, mux       )
    let g   = exactYhat( n_schedi, scals, babaici, mux + eps )
    let h   = exactYhat( n_schedi, scals, babaici, mux - eps )
    let df  = 0.5 * ( g - h ) / eps

    let sqrt2sigmax = sqrt32(2.0) * sigmax

    let tmps = map (
                    \(quad:  (f32,f32) ): f32  ->
                        let (x_quad, w_quad) = quad
                        let x = sqrt2sigmax*x_quad + mux

                        ------------------------------------------/
                        -- BEGIN function integrand(x) inlined
                        ------------------------------------------/
                        let tmp = (x - mux) / sigmax
                        let t1  = exp32( -0.5 * tmp * tmp )

                        let yhat_x = f + df*(x - mux)
                        let h1  = ( (yhat_x - muy) / sigmay_rhoxycs ) - t2*( x - mux )

                        let tmps= map (\(bbit1cstscale:  (f32,f32,f32) ): f32  ->
                                            let (bbii, t1_csti, scalei) = bbit1cstscale
                                            let h2 = h1 + bbii * sigmay_rhoxycs in
                                                t1_csti * exp32(scalei*x) * uGaussian_P(-h2)
                                        ) (zip bbi (t1_cst) scale
                                     )
                        let accum = reduce (+) (0.0) tmps
                        let integrand_res = t1 * ( uGaussian_P(-h1) - accum )
                        ------------------------------------------/
                        -- END   function integrand(x) inlined
                        ------------------------------------------/

                        in w_quad * integrand_res

                  ) (zip (x_quads) (w_quads)
                  )
    let sum = reduce (+) (0.0) tmps      in
            zc_mat * ( sum / sqrt32( pi() ) )


--------------------------
-- Root finder
--------------------------
fun exactYhat(n_schedi:  int,
                    scals: (f32,f32,f32,f32,f32,f32,f32,f32),
                    babaicis: [](f32,f32,f32,f32),
                    x: f32
                  ): f32 =
    -- ugaussian_Pinv(k)=1.0e-4
    let k=-3.71901648545568


    let uplos = map (\(babaici: (f32,f32,f32,f32)): (f32,f32)  ->
                        let (bai,bbi,aici,log_aici) = babaici
                        let baix                    = bai * x in
                            (   aici * exp32( -baix ),
                                (log_aici-baix) / bbi
                            )
                      ) babaicis
    let (ups, los) = unzip(uplos)
    let (up,  lo ) = reduce (\(x: (f32,f32)) (y: (f32,f32)): (f32,f32)  ->
                               let (a1, b1) = x
                               let (a2, b2) = y in
                               (a1 + a2, max(b1, b2) )
                           ) (0.0, -infinity()) uplos
--    let up = reduce((+), 0.0, ups)           in
--    let lo = reduce(max, -infinity(), los)    in

    let (bai, bbi, aici, log_aici) = unzip(babaicis) in

    if(n_schedi < 2) -- == 1
    then lo
    else
         let log_s = log32(up)
         let tmp   = log_s / bbi[n_schedi-1]
         let up    = if( tmp<= 0.0 ) then tmp
                     else
                       let tmp = log_s/bbi[0] in
                       if(0.0<= tmp) then tmp
                       else -infinity()

         let yl = lo - epsilon()
         let yu = up + epsilon()

         let (b, sigmax, sigmay, rhoxy, rhoxyc, rhoxycs, mux, muy) = scals

         -- y01 x = y0, y1 / phi(h_i(x, y0)) <= epsilon, 1 - phi(h_i(x, y1)) <= epsilon
         let y0 = sigmay * (rhoxy*(x-mux)/sigmax+k*rhoxycs) - rhoxyc/b + muy
         let y1 = sigmay * (rhoxy*(x-mux)/sigmax-k*rhoxycs) + muy             in

         if      (y1 <= yl) then y1 + 1.0  -- yhat is greater than y1 -> 1 - phi(h_i(x, yhat)) < epsilon
         else if (yu <= y0) then y0 - 1.0  -- yhat is lower than y0 -> phi(h_i(x, yhat)) < epsilon)
         else
              ---- `scales' is the same as `ups', however, if this branch
              ---- is not oftenly taken, it might make sense to duplicate computation,
              ---- since if the previous `ups' is in a map-reduce pattern!
              --let scales  = map(  \f32 ( (f32,f32) baiaici) ->
              --                      let (bai,aici) = baiaici in
              --                      aici * exp( -bai * x )
              --                    , zip(bai, aici)
              --                 )        in
              let scales  = ups
              let root_lb = max(yl, y0)
              let root_ub = min(yu, y1)
              let (root, iteration, error) =
                    rootFinding_Brent(1, zip scales bbi, root_lb, root_ub, 1.0e-4, 1000) in

              if      ( error == -infinity() ) then y0 - 1.0
              else if ( error ==  infinity() ) then y1 + 1.0
              else                                  root

------------------------------------------------------
----   Date: Gregorian calendar
------------------------------------------------------

fun mod(x: int, y: int): int = x - (x/y)*y

fun hours_in_dayI   (): int = 24
fun minutes_in_dayI (): int = hours_in_dayI() * 60
fun minutes_to_noonI(): int = (hours_in_dayI() / 2) * 60

fun minutes_in_day  (): f32 = 24.0*60.0


--                           year*month*day*hour*mins
fun date_of_gregorian(date:  (int,int,int,int,int)): int =
    let (year, month, day, hour, mins) = date
    let ym =
        if(month == 1 || month == 2)
        then    ( 1461 * ( year + 4800 - 1 ) ) / 4 +
                  ( 367 * ( month + 10 ) ) / 12 -
                  ( 3 * ( ( year + 4900 - 1 ) / 100 ) ) / 4
        else    ( 1461 * ( year + 4800 ) ) / 4 +
                  ( 367 * ( month - 2 ) ) / 12 -
                  ( 3 * ( ( year + 4900 ) / 100 ) ) / 4
    let tmp = ym + day - 32075 - 2444238

            in tmp * minutes_in_dayI() + hour * 60 + mins


fun gregorian_of_date (minutes_since_epoch:  int ): (int,int,int,int,int) =
    let jul = minutes_since_epoch / minutes_in_dayI()
    let l = jul + 68569 + 2444238
    let n = ( 4 * l ) / 146097
    let l = l - ( 146097 * n + 3 ) / 4
    let i = ( 4000 * ( l + 1 ) ) / 1461001
    let l = l - ( 1461 * i ) / 4 + 31
    let j = ( 80 * l ) / 2447
    let d = l - ( 2447 * j ) / 80
    let l = j / 11
    let m = j + 2 - ( 12 * l )
    let y = 100 * ( n - 49 ) + i + l

    --let daytime = minutes_since_epoch mod minutes_in_day in
    let daytime = mod( minutes_since_epoch, minutes_in_dayI() ) in

    if ( daytime == minutes_to_noonI() )

    --then [year = y; month = m; day = d; hour = 12; minute = 0]
    then (y, m, d, 12, 0)

    --else [year = y; month = m; day = d; hour = daytime / 60; minute = daytime mod 60]
    else (y, m, d, daytime / 60, mod(daytime, 60) )


fun check_date(year: int, month: int, day: int): bool =
    let tmp1 = ( 1 <= day && 1 <= month && month <= 12 && 1980 <= year && year <= 2299 )
    let tmp2 = ( day <= 28 )

    let tmp3 = if      ( month == 2 )
               then let tmpmod = mod(year, 100) in
                        ( day == 29 && mod(year, 4) == 0 && ( year == 2000 || (! (tmpmod == 0)) ) )
               else if ( month == 4 || month == 6 || month == 9 || month == 11 )
                    then ( day <= 30 )
                    else ( day <= 31 )

        in tmp1 && (tmp2 || tmp3)


fun days_between(t1: f32, t2: f32): f32 =
  (t1 - t2) / minutes_in_day()

fun date_act_365(t1: f32, t2: f32): f32 = days_between(t1, t2) / 365.0

fun leap(y: int): bool = ( mod(y,4) == 0  && ( (!(mod(y,100)==0)) || (mod(y,400)==0) ) )

fun end_of_month(year: int, month: int): int =
    if      ( month == 2 && leap(year) )                           then 29
    else if ( month == 2)                                          then 28
    else if ( month == 4 || month == 6 || month == 9 || month == 11 ) then 30
    else                                                               31


fun add_months (date:  f32, rnbmonths: f32 ): f32 =
    let nbmonths          = int(rnbmonths)
    let (y, m, d, h, min) = gregorian_of_date( int(date) )
    let m = m + nbmonths
    let (y, m) = (y + (m-1) / 12, mod(m-1, 12) + 1)
    let (y, m) = if (m <= 0) then (y - 1, m + 12) else (y, m)
    let resmin = date_of_gregorian ( (y, m, minI( d, end_of_month(y, m) ), 12, 0) ) in

            f32(resmin)


fun add_years(date: f32, nbyears: f32): f32 =
        add_months(date, nbyears * 12.0)


--assert max_date==168307199, a.k.a. "2299-12-31T23:59:59"
fun max_date (): f32 = 168307199.0

--assert min_date==3600, a.k.a., "1980-01-01T12:00:00"
fun min_date (): f32 = 3600.0

-- Date.of_string("2012-01-01")
fun today    (): f32 = f32( date_of_gregorian( (2012, 1, 1, 12, 0) ) )

fun int grayCode(int x) = (x >> 1) ^ x

////////////////////////////////////////
/// Sobol Generator
////////////////////////////////////////
fun int index([int] arr, int i) = arr[i]

fun bool testBit(int n, int ind) =
    let t = (1 << ind) in (n & t) == t

/////////////////////////////////////////////////////////////////
//// DIFFICULT VERSION: filter is hoisted outside map:
////         less computation but variable array size!
/////////////////////////////////////////////////////////////////
//fun int xorInds([int] indices, [int] dir_vs ) =
//    reduce( ^, 0, map( index(dir_vs), indices ) )
//
//fun [int] sobolIndI ( int bits_num, [[int]] dir_vs, int n ) =
//    let bits    = iota   ( bits_num ) in
//    let indices = filter ( testBit(grayCode(n)), bits )
//    in map( xorInds(indices), dir_vs )


/////////////////////////////////////////////////////////////////
//// EASY VERSION: filter is redundantly computed inside map:
////    more computation but a redofilt pattern, i.e., array
////    not instantiated!
/////////////////////////////////////////////////////////////////
fun int xorInds(int bits_num, int n, [int] dir_vs ) =
    let bits    = iota   ( bits_num )                   in
    let indices = filter ( testBit(grayCode(n)), bits ) in
    reduce( ^, 0, map( index(dir_vs), indices ) )

fun [int] sobolIndI ( int bits_num, [[int]] dir_vs, int n ) =
    map( xorInds(bits_num, n), dir_vs )
////////////////////////////////////////////////////////////////

fun [real] sobolIndR( int bits_num, [[int]] dir_vs, int n ) =
    let divisor = 2.0 pow toReal (bits_num)        in
    let arri    = sobolIndI( bits_num, dir_vs, n ) in
        map( fn real (int x) => toReal(x) / divisor, arri )

////////////////////////////////////////
/// Inverse Gaussian
////////////////////////////////////////

//tmp_rat_evalL :: SpecReal -> [SpecReal] -> SpecReal
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
        if ( ( (dp < 0.0) && (0.0 - dp <= 0.425) ) || ( (0.0 <= dp) && (dp <= 0.425) ) )
        then smallcase(dp)
        else let pp = if(dp < 0.0) then dp + 0.5 else 0.5 - dp      in
             let r  = sqrt( - log(pp) )                             in
             let x = if(r <= 5.0) then intermediate(r) else tail(r) in
                if(dp < 0.0) then 0.0 - x else x

// Transforms a uniform distribution [0,1) into a gaussian distribution (-inf, +inf)
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

//  let gauss[ bi[0]-1 ] = sd[0] * gauss[0]  in
    let bbrow = copy(replicate(num_dates, 0.0))   in
    let bbrow[ bi[0]-1 ] = sd[0] * gauss[0] in

    loop (bbrow) =
        for i < num_dates-1 do  // use i+1 since i in 1 .. num_dates-1
            let j  = li[i+1] - 1 in
            let k  = ri[i+1] - 1 in
            let l  = bi[i+1] - 1 in

            let wk = bbrow [k  ] in
            let zi = gauss [i+1] in
            let tmp= rw[i+1] * wk + sd[i+1] * zi in

            let bbrow[ l ] = if( (j + 1) == 0)   // if(j=-1)
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
      transpose( map( brownianBridgeDates(num_dates, bb_inds, bb_data), gauss2dT ) )


/////////////////////////////////
/// Black-Scholes
/////////////////////////////////

fun real zwop(real a, real b, int j) = a * b

fun [real] take(int n, [real] a) = let {first,_} = split((n), a) in first

fun [real] fftmp(int num_paths, [[real]] md_c, [real] zi) =
    map( fn real (int j) =>
            let x = map(zwop, zip(take(j+1,zi), take(j+1,md_c[j]), iota(j+1)))
            in  reduce(+, 0.0, x),
         iota(num_paths)
       )

fun [[real]] correlateDeltas(int num_paths, [[real]] md_c, [[real]] zds) =
    map( fftmp(num_paths, md_c), zds )

fun [real] combineVs([real] n_row, [real] vol_row, [real] dr_row) =
    map( +, zip(dr_row, map( *, zip(n_row, vol_row ) )))

fun [[real]] mkPrices ([real] md_starts, [[real]] md_vols, [[real]] md_drifts, [[real]] noises) =
    let e_rows = map( fn [real] ([real] x) => map(exp, x),
                      map(combineVs, zip(noises, md_vols, md_drifts))
                    )
    in  scan( fn [real] ([real] x, [real] y) => map(*, zip(x, y)), md_starts, e_rows )

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

fun real main(int contract_number,
              int num_mc_it,
              int num_dates,
              int num_und,
              int num_models, // Unused?
              int num_bits,
              [[int]] dir_vs,
              [[real]] md_c,
              [[real]] md_vols,
              [[real]] md_drifts,
              [real] md_st,
              [real] md_dv, // Unused?
              [real] md_disc,
              [[int]] bb_inds,
              [[real]] bb_data) =
    let sobol_mat = map ( sobolIndR(num_bits, dir_vs), map(fn int (int x) => x + 1, iota(num_mc_it)) ) in
    let gauss_mat = map ( ugaussian, sobol_mat )                                       in
    let bb_mat    = map ( brownianBridge( num_und, num_dates, bb_inds, bb_data ), gauss_mat )    in
    let bs_mat    = map ( blackScholes( num_und, md_c, md_vols, md_drifts, md_st ), bb_mat ) in

    let payoffs   = map ( payoff2(md_disc), bs_mat ) in
    let payoff    = reduce ( +, 0.0, payoffs )       in
    payoff / toReal(num_mc_it)

////////////////////////////////////////
// PAYOFF FUNCTION
////////////////////////////////////////

fun real payoff2 ([real] md_disc, [[real]] xss) =
// invariant: length(xss) == 5, i.e., 5x3 matrix
    let divs    = [ 1.0/3758.05, 1.0/11840.0, 1.0/1200.0 ]             in
    let xss_div = map( fn [real] ([real] xs) => map(*, zip(xs, divs)), xss     ) in
    let mins    = map( MIN, xss_div )
    in  if( 1.0 <= mins[0] ) then trajInner(1150.0, 0, md_disc)
        else if( 1.0 <= mins[1] ) then trajInner(1300.0, 1, md_disc)
             else if( 1.0 <= mins[2] ) then trajInner(1450.0, 2, md_disc)
                  else if( 1.0 <= mins[3] ) then trajInner(1600.0, 3, md_disc)
                       else if( 1.0 <= mins[4] ) then trajInner(1750.0, 4, md_disc)
                            else if( 0.75 < mins[4] ) then trajInner(1000.0, 4, md_disc)
                                 else trajInner(1000.0 * mins[4], 4, md_disc)

fun real MIN([real] arr) =
  reduce( fn real (real x, real y) => if(x<y) then x else y, arr[0], arr )

fun real trajInner(real amount, int ind, [real] disc) = amount * disc[ind]

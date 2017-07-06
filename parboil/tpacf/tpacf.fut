-- An implementation of the tpacf benchmark from the parboil benchmark suite
--
-- ==
-- compiled input @ data/small.in
-- output @ data/small.out
-- compiled input @ data/medium.in
-- output @ data/medium.out
-- compiled input @ data/large.in
-- output @ data/large.out

import "/futlib/math"
import "/futlib/array"

type vec3 = (f64, f64, f64)

let pi(): f64 = 3.1415926535897932384626433832795029f64
let dec2rad(dec: f64): f64 = pi()/180.0f64 * dec
let rad2dec(rad: f64): f64 = 180.0f64/pi() * rad
let min_arcmin(): f64 = 1.0f64
let max_arcmin(): f64 = 10000.0f64
let bins_per_dec(): f64 = 5.0f64
let numBins(): i32 = 20

let iota32(num: i32): [num]f64 =
    map f64 (iota(num))

-- Prøv stream_red i stedet
let sumBins(bins: [#numBinss][#numBins]i32): *[numBins]i32 =
    map (\(binIndex: []i32): i32  -> reduce (+) 0i32 binIndex) (transpose(bins))

let log10(num: f64): f64 = f64.log(num) / f64.log(10.0)

let doCompute(data1: 
    [#num1]vec3,
    data2: [#num2]vec3,
    numBins: i32,
    numBins2: i32,
    binb: [#numBBins]f64
): *[numBins2]i32 =
    let value = map (\(xOuter: f64, yOuter: f64, zOuter: f64): *[numBins2]i32  ->
            stream_map (\(inner: [#chunk]vec3): *[numBins2]i32  ->
                    loop dBins = replicate numBins2 0i32 for i < chunk do
                        let (xInner, yInner, zInner) = inner[i]
                        let dot = xOuter * xInner + yOuter * yInner + zOuter * zInner
                        let (min,max) =
                          loop (min, max) = (0, numBins) while (min+1) < max do
                            let k = (min+max) / 2
                            in unsafe if dot >= binb[k]
                                      then (min, k)
                                      else (k, max)

                        let index = unsafe if dot >= binb[min]
                                    then min
                                    else if dot < binb[max]
                                        then max+1
                                        else max

                        in unsafe let dBins[index] = dBins[index] + 1i32 in dBins
                ) data2
        ) data1

    in sumBins(value)

let doComputeSelf(data: 
    [#numD]vec3,
    numBins: i32,
    numBins2: i32,
    binb: [#numBBins]f64
): *[numBins2]i32 =
-- loop version
    let value = map (\(vec: vec3, index: i32): [numBins2]i32  ->
                    let (xOuter, yOuter, zOuter) = vec
                    in loop dBins = replicate numBins2 0i32 for j in range (index+1) numD 1 do
                        let (xInner, yInner, zInner) = data[j]
                        let dot = xOuter * xInner + yOuter * yInner + zOuter * zInner
                        let (min,max) = loop (min, max) = (0, numBins) while (min+1) < max do
                            let k = (min+max) / 2
                            in unsafe if dot >= binb[k]
                                      then (min, k)
                                      else (k, max)

                        let index = unsafe if dot >= binb[min]
                                    then min
                                    else if dot < binb[max]
                                        then max+1
                                        else max

                        in unsafe let dBins[index] = dBins[index] + 1i32 in dBins
                ) (zip data (iota(numD)))

    in sumBins(value)

let fixPoints(ra: f64, dec: f64): vec3 =
    let rarad = dec2rad(ra)
    let decrad = dec2rad(dec)
    let cd = f64.cos(decrad)

    in (f64.cos(rarad)*cd, f64.sin(rarad)*cd, f64.sin(decrad))

let main(datapointsx: 
    [#numD]f64,
    datapointsy: [#numD]f64,
    randompointsx: [#numRs][#numR]f64,
    randompointsy: [#numRs][#numR]f64
): *[60]i32 =
    let numBins2 = numBins() + 2
    let binb = map (\(k: f64): f64  ->
                        f64.cos((10.0 ** (log10(min_arcmin()) + k*1.0/bins_per_dec())) / 60.0 * dec2rad(1.0))) (
                    iota32(numBins() + 1))
    let datapoints = map fixPoints (zip datapointsx datapointsy)
    let randompoints = map (\(x: [#numR]f64, y: [#numR]f64): [numR]vec3  ->
                            map fixPoints (zip x y)) (
                           zip randompointsx randompointsy)
    let (rrs, drs) = unzip(map (\(random: [#numR]vec3): (*[]i32, *[]i32)  ->
                                (doComputeSelf(random, numBins(), numBins2, binb),
                                doCompute(datapoints, random, numBins(), numBins2, binb))) randompoints)
    let (res,_,_,_) = loop (res, dd, rr, dr) =
                           (replicate (numBins()*3) 0i32,
                            doComputeSelf(datapoints, numBins(), numBins2, binb),
                            sumBins(rrs),
                            sumBins(drs)) for i < numBins() do
        let res[i*3] = dd[i+1]
        let res[i*3+1] = dr[i+1]
        let res[i*3+2] = rr[i+1]

        in (res, dd, rr, dr)

    in res

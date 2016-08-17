-- An implementation of the tpacf benchmark from the parboil benchmark suite
--
-- ==
-- compiled input @ small/input-data
-- output @ small/output-data
-- compiled input @ medium/input-data
-- output @ medium/output-data
-- compiled input @ large/input-data
-- output @ large/output-data

--default(f32)

type vec3 = (f64, f64, f64)

fun f64 pi() = 3.1415926535897932384626433832795029f64
fun f64 dec2rad(f64 dec) = pi()/180.0f64 * dec
fun f64 rad2dec(f64 rad) = 180.0f64/pi() * rad
fun f64 min_arcmin() = 1.0f64
fun f64 max_arcmin() = 10000.0f64
fun f64 bins_per_dec() = 5.0f64
fun i32 numBins() = 20

fun [num]f64 iota32(i32 num) =
    map(f64, iota(num))

-- Prøv streamRed i stedet
fun *[numBins]i32 sumBins([numBinss][numBins]i32 bins) =
    map(fn i32 ([]i32 binIndex) => reduce(+, 0i32, binIndex), transpose(bins))

fun f64 log10(f64 num) = log64(num) / log64(10.0)

fun *[numBins2]i32 doCompute(
    [num1]vec3 data1,
    [num2]vec3 data2,
    i32 numBins,
    i32 numBins2,
    [numBBins]f64 binb
) =
    let value = map(fn *[numBins2]i32 (f64 xOuter, f64 yOuter, f64 zOuter) =>
            streamMap(fn *[numBins2]i32 (int chunk, []vec3 inner) =>
                    loop (dBins = replicate(numBins2, 0i32)) = for i < chunk do
                        let (xInner, yInner, zInner) = inner[i]
                        let dot = xOuter * xInner + yOuter * yInner + zOuter * zInner
                        loop ((min, max) = (0, numBins)) = while (min+1) < max do
                            let k = (min+max) / 2 in
                            unsafe if dot >= binb[k]
                            then (min, k)
                            else (k, max)
                        in
                        let index = unsafe if dot >= binb[min]
                                    then min
                                    else if dot < binb[max]
                                        then max+1
                                        else max
                        in
                        unsafe let dBins[index] = dBins[index] + 1i32 in dBins
                    in dBins
                , data2)
        , data1)
    in
    sumBins(value)

fun *[numBins2]i32 doComputeSelf(
    [numD]vec3 data,
    i32 numBins,
    i32 numBins2,
    [numBBins]f64 binb
) =
-- loop version
    let value = map(fn [numBins2]i32 (vec3 vec, i32 index) =>
                    let (xOuter, yOuter, zOuter) = vec
                    loop (dBins = replicate(numBins2, 0i32)) = for (index+1) <= j < numD do
                        let (xInner, yInner, zInner) = data[j]
                        let dot = xOuter * xInner + yOuter * yInner + zOuter * zInner
                        loop ((min, max) = (0, numBins)) = while (min+1) < max do
                            let k = (min+max) / 2 in
                            unsafe if dot >= binb[k]
                            then (min, k)
                            else (k, max)
                        in
                        let index = unsafe if dot >= binb[min]
                                    then min
                                    else if dot < binb[max]
                                        then max+1
                                        else max
                        in
                        unsafe let dBins[index] = dBins[index] + 1i32 in dBins
                    in dBins
                , zip(data, iota(numD)))
    in
    sumBins(value)

fun vec3 fixPoints(f64 ra, f64 dec) =
    let rarad = dec2rad(ra)
    let decrad = dec2rad(dec)
    let cd = cos64(decrad)
    in
    (cos64(rarad)*cd, sin64(rarad)*cd, sin64(decrad))

fun *[60]i32 main(
    [numD]f64 datapointsx,
    [numD]f64 datapointsy,
    [numRs][numR]f64 randompointsx,
    [numRs][numR]f64 randompointsy
) =
    let numBins2 = numBins() + 2
    let binb = map(fn f64 (f64 k) =>
                        cos64((10.0 ** (log10(min_arcmin()) + k*1.0/bins_per_dec())) / 60.0 * dec2rad(1.0)),
                    iota32(numBins() + 1))
    let datapoints = map(fixPoints, zip(datapointsx, datapointsy))
    let randompoints = map(fn [numR]vec3 ([numR]f64 x, [numR]f64 y) =>
                            map(fixPoints, zip(x,y)),
                           zip(randompointsx, randompointsy))
    let (rrs, drs) = unzip(map(fn (*[]i32, *[]i32) ([numR]vec3 random) =>
                                (doComputeSelf(random, numBins(), numBins2, binb),
                                doCompute(datapoints, random, numBins(), numBins2, binb)),
                                randompoints))
    loop ((res, dd, rr, dr) = (replicate(numBins()*3, 0i32),
                               doComputeSelf(datapoints, numBins(), numBins2, binb),
                               sumBins(rrs),
                               sumBins(drs))) = for i < numBins() do
        let res[i*3] = dd[i+1]
        let res[i*3+1] = dr[i+1]
        let res[i*3+2] = rr[i+1]
        in
        (res, dd, rr, dr)
    in
    res

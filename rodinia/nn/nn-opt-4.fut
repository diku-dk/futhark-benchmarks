-- Code and comments based on
-- https://github.com/kkushagra/rodinia/blob/master/openmp/nn
--
-- ==
--
-- input @ data/medium.in
-- output @ data/medium.out

import "/futlib/math"
import "/futlib/array"

let infty : f32 = 1.0f32 / 0.0f32
let emptyRecord : (i32, f32) = (0, 0.0f32)

let stride = 4i32

let findMinNotIn (a : [stride](f32,i32)) (b : [stride](f32,i32)) (k : i32) : (f32,i32) =
    loop((m,ind) = (infty,-1)) for i < stride do
        let (a_v, a_i) = a[i] in
        if a_v >= m then (m,ind)
        else let ok = true
             let ok = loop (ok) for j < k do
                let (_, b_i) = b[j]
                in  ok && (b_i != a_i)
             in if ok then (a_v, a_i) else (m, ind)

let main(resultsCount:    i32, lat: f32, lng: f32, 
        locations_lat: [#numRecords]f32, 
        locations_lng: [#numRecords]f32    ): ([]i32, []f32) =
  let locations    = zip (locations_lat) (locations_lng)
  -- let resultsCount = if (resultsCount > numRecords) then numRecords else resultsCount

  let loopCount = (resultsCount + (stride-1)) / stride
  let padResultsCount = loopCount * stride
  let padNumRecords = ((numRecords + (stride-1)) / stride) * stride
  let numQuads  = padNumRecords/stride

  let distances = 
      map (\(i : i32): f32  -> unsafe
                if i >= numRecords then infty else
                let (lat_i, lng_i) = locations[i]
                in f32.sqrt( (lat-lat_i)*(lat-lat_i) + (lng-lng_i)*(lng-lng_i) )
         ) (iota padNumRecords)

  let (results_ind, results_dst) = unzip( reshape (loopCount, stride) (replicate padResultsCount emptyRecord)  )

  let ne_v  = replicate stride infty 
  let ne_vi = zip ne_v (replicate stride (-1))

  let (results_ind, results_dst, distances) =
    loop ((results_ind, results_dst, distances))
    for i < loopCount do
        -- let arrinds = reshape (numQuads,stride) (zip distances (iota padNumRecords))
        let arrinds = map (\arr k-> zip arr ([stride*k, stride*k+1, stride*k+2, stride*k+3]) )
                          (reshape (numQuads,stride) distances) (iota numQuads)
        let minDistsLocs =
            reduce_comm (\(di1: [stride](f32,i32)) (di2: [stride](f32,i32)) : [stride](f32, i32) ->
                            loop (res = replicate stride (infty, -1)) for k < stride do
                                let (d1, i1) = findMinNotIn di1 res k
                                let (d2, i2) = findMinNotIn di2 res k
                                let di = if(d1 < d2) then (d1, i1) 
                                         else if (d2 < d1) then (d2, i2)
                                         else if (i1 < i2) then (d1, i1) 
                                         else                   (d2, i2)
                                let res[k] = di
                                in  res
                        ) ne_vi arrinds
        let (minDists, minLocs) = unzip minDistsLocs

        let distances = scatter distances minLocs ne_v
        let results_ind[i] = minLocs
        let results_dst[i] = minDists
        in (results_ind, results_dst, distances)

  in ( reshape (padResultsCount) results_ind
     , reshape (padResultsCount) results_dst )

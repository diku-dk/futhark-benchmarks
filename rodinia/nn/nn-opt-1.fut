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

let main(resultsCount:    i32, lat: f32, lng: f32, 
        locations_lat: [#numRecords]f32, 
        locations_lng: [#numRecords]f32    ): ([resultsCount]i32, [resultsCount]f32) =
  let locations    = zip (locations_lat) (locations_lng)
  -- let resultsCount = if (resultsCount > numRecords) then numRecords else resultsCount
  let distances = reshape (numRecords,1)
       ( map (\(latlng: (f32,f32)): f32  ->
                let (lat_i, lng_i) = latlng
                in f32.sqrt( (lat-lat_i)*(lat-lat_i) + (lng-lng_i)*(lng-lng_i) )
             ) locations )

  let (results_ind, results_dst) = unzip( replicate resultsCount emptyRecord )
  let results_ind = reshape (resultsCount,1) results_ind
  let results_dst = reshape (resultsCount,1) results_dst

  let ne_v  = [infty]
  let ne_iv = (ne_v, [0])

  let (results_ind, results_dst, distances) =
    loop ((results_ind, results_dst, distances))
    for i < resultsCount do
        let (minDist, minLoc) = 
            reduce_comm (\(di1: ([1]f32,[1]i32)) (di2: ([1]f32,[1]i32)): ([1]f32, [1]i32)  ->
                            let( (da1, ia1), (da2,ia2) ) = ( di1, di2 )
                            let ( d1, i1, d2, i2 ) = (da1[0], ia1[0], da2[0], ia2[0])
                            in if(d1 < d2)       then di1 
                               else if (d2 < d1) then di2
                               else if (i1 < i2) then di1 else di2
                        ) ne_iv (zip distances (reshape (numRecords,1) (iota numRecords)) )

        let distances[minLoc[0]] = ne_v
        let results_ind[i] = minLoc
        let results_dst[i] = minDist
        in (results_ind, results_dst, distances)
  in (reshape (resultsCount) results_ind, reshape (resultsCount) results_dst)

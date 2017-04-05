-- Code and comments based on
-- https://github.com/kkushagra/rodinia/blob/master/openmp/nn
--
-- ==
--
-- input @ data/medium.in
-- output @ data/medium.out

import "futlib/math"

let infty(): f32 = 1.0f32 / 0.0f32
let emptyRecord(): (i32, f32) = (0, 0.0f32)

let main(resultsCount:    i32, lat: f32, lng: f32, 
        locations_lat: [#numRecords]f32, 
        locations_lng: [#numRecords]f32    ): ([resultsCount]i32, [resultsCount]f32) =
  let locations    = zip (locations_lat) (locations_lng)
  -- let resultsCount = if (resultsCount > numRecords) then numRecords else resultsCount
  let distances = 
      map (\(latlng: (f32,f32)): f32  ->
                let (lat_i, lng_i) = latlng
                in f32.sqrt( (lat-lat_i)*(lat-lat_i) + (lng-lng_i)*(lng-lng_i) )
         ) locations

  let (results_ind, results_dst) = unzip( copy(replicate resultsCount (emptyRecord()))  )
  loop ((results_ind, results_dst, distances)) = 
    for i < resultsCount do
        let (minDist, minLoc) = 
            reduce_comm (\(di1: (f32,i32)) (di2: (f32,i32)): (f32, i32)  ->
                            let( (d1, i1), (d2,i2) ) = ( di1, di2 )
                            in if(d1 < d2) then (d1, i1) 
                               else if (d2 < d1) then (d2, i2)
                               else if (i1 < i2) then (d1, i1) else (d2, i2)
                      ) (infty(), 0) (zip distances (iota(numRecords)) )

        let distances[minLoc] = infty()
        let results_ind[i] = minLoc
        let results_dst[i] = minDist
        in (results_ind, results_dst, distances)
  in (results_ind, results_dst)

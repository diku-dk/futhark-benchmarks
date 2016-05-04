-- Code and comments based on
-- https://github.com/kkushagra/rodinia/blob/master/openmp/nn
--
-- ==
--
-- notravis input @ data/medium.in
-- output @ data/medium.out

fun f32 infty() = 1.0f32 / 0.0f32
fun (int, f32) emptyRecord() = (0, 0.0f32)

fun ([int, resultsCount], [f32, resultsCount])
main(   int resultsCount, f32 lat, f32 lng, 
        [f32, numRecords] locations_lat, 
        [f32, numRecords] locations_lng    ) =
  let locations    = zip(locations_lat, locations_lng) in
  -- let resultsCount = if (resultsCount > numRecords) then numRecords else resultsCount in
  let distances = 
      map(  fn f32 ((f32,f32) latlng) =>
                let (lat_i, lng_i) = latlng in
                sqrt32( (lat-lat_i)*(lat-lat_i) + (lng-lng_i)*(lng-lng_i) )
         ,  locations )
  in
  let (results_ind, results_dst) = unzip( copy(replicate( resultsCount, emptyRecord() ))  ) in
  loop ((results_ind, results_dst, distances)) = 
    for i < resultsCount do
        let (minDist, minLoc) = 
            reduceComm( fn (f32, int) ((f32,int) di1, (f32,int) di2) =>
                            let( (d1, i1), (d2,i2) ) = ( di1, di2 ) in
                            if(d1 < d2) then (d1, i1) 
                            else if (d2 < d1) then (d2, i2)
                                 else if (i1 < i2) then (d1, i1) else (d2, i2)
                      , (infty(), 0), zip(distances, iota(numRecords)) )
        in
        let distances[minLoc] = infty() in
        let results_ind[i] = minLoc     in
        let results_dst[i] = minDist    in
        (results_ind, results_dst, distances)
  in (results_ind, results_dst)

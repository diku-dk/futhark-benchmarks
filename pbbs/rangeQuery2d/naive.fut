-- A naive implementation that uses a brute force O(n*m) algorithm. Because of
-- how slow it is, we do not run it on the largr workloads.
--
-- Originally written by Shatin Nguyen.
-- ==
-- input @ data/2DinSphere_10K.in
-- output @ data/2DinSphere_10K.out
-- input @ data/2DonSphere_10K.in
-- output @ data/2DonSphere_10K.out
-- input @ data/2Dkuzmin_10K.in
-- output @ data/2Dkuzmin_10K.out
--
-- notest input @ data/2DinSphere_100K.in
-- notest input @ data/2DonSphere_100K.in
-- notest input @ data/2Dkuzmin_100K.in
--
-- notest input @ data/2DinSphere_1M.in
-- notest input @ data/2DonSphere_1M.in
-- notest input @ data/2Dkuzmin_1M.in

import "util"

def naive [n] [m] (points: [n]point) (rectangles: [m]rectangle) : [m]i32 =
  map (\r ->
         i32.sum (map (\p ->
                         let px = p[0]
                         let py = p[1]
                         let rx0 = r[0]
                         let ry0 = r[1]
                         let rx1 = r[2]
                         let ry1 = r[3]
                         let rx0' = f64.min rx0 rx1
                         let ry0' = f64.min ry0 ry1
                         let rx1' = f64.max rx0 rx1
                         let ry1' = f64.max ry0 ry1
                         let check_x = rx0' <= px && rx1' >= px
                         let check_y = ry0' <= py && ry1' >= py
                         in if check_x && check_y
                            then 1
                            else 0)
                      points))
      rectangles

entry main (all: []point) : []i32 =
  let (points, rectangles) = split_input all
  in naive points rectangles

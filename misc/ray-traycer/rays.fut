-- Simple ray creation test
--
-- ==
-- tags { futhark-c futhark-opencl }
-- compiled input @ rays3.in

import "vectors"
import "transform"

type int2 = (i32, i32)
type int3 = (i32, i32, i32)
type double2 = (f64, f64)
type double3 = (f64, f64, f64)
type ray = (double3, double3, int2)
type hit = (f64, f64, f64, f64, f64, f64)

val DBL_MAX: f64 = 1.7976931348623158e308
val DBL_EPS: f64 = 1.11022302462515655e-13

val NULL_HIT: hit = (DBL_MAX, 0.0, 0.0, 0.0, 0.0, 0.0)

-- Extracts the origin from a ray
fun rayOri((o, d, xy): ray): double3 = o

-- Extracts the direction from a ray
fun rayDir((o, d, xy): ray): double3 = d

-- Moves the point o along direction d for distance t
fun moveAlong(o: double3, d: double3, t: f64): double3 = o +^ d *^ t

-- Just a quick transform for ease-of-use, from array to a three-long f64 tuple
fun arrayTo3Tuple(arr: []f64): (f64, f64, f64) = unsafe (arr[0], arr[1], arr[2])

-- Technically not a table, but this function creates a list of rays (origins and directions),
--   and the pixels they each represent.
fun createRayTable(dimensionsInPixels: int2, unitsPerDimensionPixel: double2,
	rightVector: double3, downVector: double3, position: double3, topLeft: double3): []ray = 
		let (pw, ph) = dimensionsInPixels
		let (xs, ys) = (iota pw, iota ph)
		
		let (uppw, upph) = unitsPerDimensionPixel
		let xds 		 = map (\x -> (f64(x)) * uppw) xs
		let yds 		 = map (\y -> (f64(y)) * upph) ys
		
		let rvs = map (\x -> topLeft -^ position +^ rightVector *^ x) xds
		let rvs = zip rvs xs

		let dvs = map (\y -> downVector *^ y) yds
		let dvs = zip dvs ys

		let rt = 
			map (\(rv, xp)
				 -> map (\(dv, yp)
						 -> (position, rv +^ dv, (xp, yp))
						) dvs
				) rvs
		
		in reshape (pw*ph) rt

-- Triangle intersection using the Möller-Trombore method
fun mollerTromboreIntersection(triangle: [][]f64, (o, d, xy): ray): hit =
	unsafe
	let A  = arrayTo3Tuple(triangle[0])
	let e1 = arrayTo3Tuple(triangle[1])
	let e2 = arrayTo3Tuple(triangle[2])
	
	let r = crossProductVector(d, e2)
	
	let s = o -^ A
	
	let a = dotProductVector(e1, r)

	let q = crossProductVector(s, e1)

	let u = dotProductVector(s, r)
	let v = dotProductVector(d, q)

	in if (
		if (a > DBL_EPS)
		then
			if (u < 0.0 || u > a) then true else (v < 0.0 || (u + v) > a)
		else
			if (a < -DBL_EPS)
			then
				if (u > 0.0 || u < a) then true else (v > 0.0 || (u + v) < a)
			else true)
	then
		NULL_HIT
	else
		let f = 1.0 / a

		let t = f * dotProductVector(e2, q)
		
		let finalU = u * f
		let finalV = v * f
		
		in if (t >= 0.0)
		then			
			let (r, g, b) = (1.0, 1.0, 1.0)
			in (t, finalU, finalV, r, g, b)
		else
			NULL_HIT
	
-- Our abstract intersection function, it takes a list of triangles and a ray, and returns
--   the first (if any) hit among the list.
fun intersect(triangles: [][][]f64, ray: ray): hit =	
	let hits = map(\tri -> mollerTromboreIntersection(tri, ray)) triangles
	
	in reduce (\(ta, ua, va, ra, ga, ba) (t, u, v, r, g, b)
			 -> if (t < ta) then (t, u, v, r, g, b) else (ta, ua, va, ra, ga, ba)
			) NULL_HIT hits
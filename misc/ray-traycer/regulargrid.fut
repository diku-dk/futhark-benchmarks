-- Ray-Tracer provided by Frederik Lundby and Alexander Bergendorff, 
-- developed during their BSc thesis at ITU with Patrick Bahr.
-- ==
-- input @ input1.in
-- output @ output1.out
--
-- input @ input3.in
-- output @ output3.out
--
-- input @ inputbench.in
-- output @ outputbench.out

import "rays"
import "bounds"

type int2 = (i32, i32)
type int3 = (i32, i32, i32)
type double3 = (f64, f64, f64)
type ray = (double3, double3, int2)
type hit = (f64, f64, f64, f64, f64, f64)

val DBL_MIN: f64 = -1.7976931348623158e308
val DBL_MAX: f64 = 1.7976931348623158e308
val DBL_LARGE_EPS: f64 = 7.450580596923828125e-9

val NULL_HIT: hit = (DBL_MAX, 0.0, 0.0, 0.0, 0.0, 0.0)

-- In a single dimension, this function determines the cell index by using
--   the entry point e, a tie breaker in the form of the direction d, and the
--   size of a cell in this dimension, c.
fun cellIdx(d: f64, e: f64, c: f64): i32 = 
	if d > 0.0 then i32((e + DBL_LARGE_EPS) / c) else i32(((e - DBL_LARGE_EPS) / c))

-- Determines the cell indexes in three dimensions at once.
fun cellIdxes((dx, dy, dz): double3, (ex, ey, ez): double3, (cx, cy, cz): double3): int3 =
	(cellIdx(dx, ex, cx), cellIdx(dy, ey, cy), cellIdx(dz, ez, cz))

-- Determines the direction and speed of our ray in one dimension, in terms
--   of cell traversal.
fun getIncAndNext(d: f64, i: i32, dtd: f64, c: f64, min: f64, o: f64): (i32, f64) =
	if (d == 0.0) then (0, dtd) else
	(
		if (d > 0.0)
		then
			let far = (f64(i) + 1.0)*c + min
			let dis = intrinsics.abs(far - o)
			in (1, intrinsics.abs(dis / d))
		else
			let far = (f64(i))*c + min
			let dis = intrinsics.abs(far - o)
			in (-1, intrinsics.abs(dis / d))
	)

-- Determines the direction and speed of our ray in all three dimensions.
fun getIncsAndNexts(
	(dx, dy, dz): double3, (ix, iy, iz): int3, (dtdx, dtdy, dtdz): double3,
	(cx, cy, cz): double3, (x1, y1, z1): double3, (ox, oy, oz): double3): (int3, double3) =
		let ((incx, nextx), (incy, nexty), (incz, nextz)) = 
			(getIncAndNext(dx, ix, dtdx, cx, x1, ox),
			 getIncAndNext(dy, iy, dtdy, cy, y1, oy),
			 getIncAndNext(dz, iz, dtdz, cz, z1, oz))
		in ((incx, incy, incz), (nextx, nexty, nextz))

-- Creates a set of data that can be used to quickly find the next cell during traversal
fun createContinuation(((o, d, xy): ray, t: f64), gridResolution: int3, c: double3, mins: double3, maxs: double3): (double3, double3, int3, int3, ray) =	
	let e = if (contains (o, mins, maxs)) then o else moveAlong(o, d, t)
	
	let e = e -^ mins

	let is = cellIdxes(d, e, c)
	
	let dts = absVector(c // d)

	let (incs, nexts) = getIncsAndNexts(d, is, dts, c, mins, o)
	
	in (nexts, dts, is, incs, (o, d, xy))

-- Traverses using a continuation
fun continueTraverse(
	((nextx, nexty, nextz): double3,
	(dtdx, dtdy, dtdz): double3,
	(ix, iy, iz): int3,
	(incx, incy, incz): int3,
	(o, d, (x, y)): ray),
	(rx, ry, rz): int3, cellTriangles: [][][]f64, cellAmounts: []i32): (hit, i32, i32) = 		
		loop (((nextx, nexty, nextz), (ix, iy, iz), hit) = ((nextx, nexty, nextz), (ix, iy, iz), NULL_HIT)) =
			while (ix >= 0 && ix < rx
				&& iy >= 0 && iy < ry
				&& iz >= 0 && iz < rz) do
					-- Body of loop
					let idx = ix * ry * rz + iy * rz + iz
					
					let ((nextx, nexty, nextz), (ix, iy, iz), maxT, hit) = 
						if (nexty < nextx)
						then
							if (nexty < nextz)
							then
								((nextx, nexty + dtdy, nextz), (ix, iy + incy, iz), nexty, hit)
							else
								((nextx, nexty, nextz + dtdz), (ix, iy, iz + incz), nextz, hit)
						else
							if (nextx < nextz)
							then
								((nextx + dtdx, nexty, nextz), (ix + incx, iy, iz), nextx, hit)
							else
								((nextx, nexty, nextz + dtdz), (ix, iy, iz + incz), nextz, hit)
					
					in unsafe
					let start = cellAmounts[idx]
					let end = cellAmounts[idx+1]
					
					let tris = cellTriangles[start:end:1]
					
					let bestHit = intersect(tris, (o, d, (x,y)))
					let (t, _, _, _, _, _) = bestHit
					
					let (ix, bestHit) = if (t < maxT) then (-1, bestHit) else (ix, hit)
			
			-- Return value from loop
			in ((nextx, nexty, nextz), (ix, iy, iz), bestHit)
		
		-- Return value from continueTraverse
		in (hit, i32(x), i32(y))
	
-- Traverses the regulargrid to find a hit with 
fun traverse(rt: []ray, gridResolution: int3, gridCellSize: double3, gridMinimums: double3, gridMaximums: double3, cellTriangles: [][][]f64, cellAmounts: []i32, transformMatrix: []f64, invertedMatrix: []f64): [][]i32 =	
	let rt = map (\ray -> transformRay(ray, invertedMatrix)) rt
	
	let rt = map (\ray -> (ray, (intersectsWithRayTs(ray, gridMinimums, gridMaximums)))) rt
			 
	let rt = filter (\(_, t) -> t >= 0.0 ) rt
	
	let ct =
		map (\rayAndT
			 -> createContinuation(rayAndT, gridResolution, gridCellSize, gridMinimums, gridMaximums)
			) rt	
	let ct =
		filter (\(_, _, _, (incx, incy, incz), _) -> !(incx == 0 && incy == 0 && incz == 0)) ct
	
	let ht = 
		map (\con
			 -> continueTraverse(con, gridResolution, cellTriangles, cellAmounts)
			) ct
	let ht = filter (\((t, _, _, _, _, _), _, _) -> t < DBL_MAX) ht

	-- Currently this map transforms our hit into just a white colour and location.
	--   This will definitely change later.
	in map (\((t, u, v, r, g, b), x, y) -> [255+1, 255, 255, x, y]) ht

-- Traces a ray and creates a colour
fun trace(rt: []ray, reflectionCount: i32, lightcount: i32, lights: [][]f64, gridResolution: int3, gridCellSize: double3, gridMinimums: double3, gridMaximums: double3, cellTriangles: [][][]f64, cellAmounts: []i32, transformMatrix: []f64, invertedMatrix: []f64): [][]i32 =
	traverse(rt, gridResolution, gridCellSize, gridMinimums, gridMaximums, cellTriangles, cellAmounts, transformMatrix, invertedMatrix)	
	-- Currently this function only traverses our spatial data structure once.
	-- In the future, this function would create hits, and those hits would require
	--   extra traces to determine shadows, reflections, depth of field, and more.

-- A very... Disgustingly huge main function.
fun main(
pixelWidth: i32, pixelHeight: i32,
rightVectorX: f64, rightVectorY: f64, rightVectorZ: f64,
downVectorX: f64, downVectorY: f64, downVectorZ: f64,
positionX: f64, positionY: f64, positionZ: f64,
topLeftX: f64, topLeftY: f64, topLeftZ: f64,
unitsPerPixelWidth: f64, unitsPerPixelHeight: f64,
lightCount: i32, lights: [][]f64,
gridResX: i32, gridResY: i32, gridResZ: i32,
gridCellW: f64, gridCellH: f64, gridCellD: f64,
gridMinX: f64, gridMinY: f64, gridMinZ: f64,
gridMaxX: f64, gridMaxY: f64, gridMaxZ: f64,
triangleCount: i32, cellTriangles: [][][]f64,
cellAmounts: []i32,
transformMatrix: []f64, invertedMatrix: []f64): [][]i32  =  
	let rt = createRayTable(
		(pixelWidth, pixelHeight), (unitsPerPixelWidth, unitsPerPixelHeight),
		(rightVectorX, rightVectorY, rightVectorZ),
		(downVectorX, downVectorY, downVectorZ),
		(positionX, positionY, positionZ),
		(topLeftX, topLeftY, topLeftZ))

	in trace(rt, 1, lightCount, lights, 
		(gridResX, gridResY, gridResZ), 
		(gridCellW, gridCellH, gridCellD), 
		(gridMinX, gridMinY, gridMinZ), 
		(gridMaxX, gridMaxY, gridMaxZ), 
		cellTriangles, cellAmounts, transformMatrix, invertedMatrix)

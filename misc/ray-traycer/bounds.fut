type int2 = (i32, i32)
type double3 = (f64, f64, f64)
type ray = (double3, double3, int2)

val DBL_MIN: f64 = -1.7976931348623158e308
val DBL_MAX: f64 = 1.7976931348623158e308

-- Checks when a ray coming from o with direction d passes through x1 and x2 in a single dimension
fun axisCheck(t1: f64, t2: f64, x2: f64, x1: f64, o: f64, d: f64): (f64, f64) =
	if (d > 0.0)
	then
		let t1p = (x1 - o) / d
		let t2p = (x2 - o) / d
		in (
			if (t1p > t1) then t1p else t1,
			if (t2p < t2) then t2p else t2
		)
	else
		let t1p = (x2 - o) / d
		let t2p = (x1 - o) / d
		in (
			if (t1p > t1) then t1p else t1,
			if (t2p < t2) then t2p else t2
		)

-- Checks if (and when) a ray intersects with a bounding box determined by (x1, y1, z1) and (x2, y2, z2)
fun intersectsWithRayTs(((ox, oy, oz), (dx, dy, dz), xy): ray, (x1, y1, z1): double3, (x2, y2, z2): double3): f64 =
	let (t1, t2) = (DBL_MIN, DBL_MAX)
	
	let (t1, t2) = axisCheck(t1, t2, x2, x1, ox, dx)
	let (t1, t2) = axisCheck(t1, t2, y2, y1, oy, dy)
	let (t1, t2) = axisCheck(t1, t2, z2, z1, oz, dz)
	
	in 
	if !(t1 == DBL_MAX || t2 == DBL_MAX && t1 < t2 && t2 > 0.0)
	then (if (t1 < 0.0) then t2 else t1) 
	else -1.0

-- Checks if a point exists within a bounding box determined by (x1, y1, z1) and (x2, y2, z2)
fun contains((x, y, z): double3, (x1, y1, z1): double3, (x2, y2, z2): double3): bool =
	!(x > x2 || x1 > x || y > y2 || y1 > y || z > z2 || z1 > z)
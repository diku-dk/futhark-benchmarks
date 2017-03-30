type int2 = (i32, i32)
type double3 = (f64, f64, f64)
type ray = (double3, double3, int2)

import "vectors"

-- Transforms a 3d point via a 4x3 matrix
fun transformPoint((x, y, z): double3, matrix: []f64): (f64, f64, f64) =
	(
		matrix[0]*x + matrix[1]*y + matrix[2] *z + matrix[3],
		matrix[4]*x + matrix[5]*y + matrix[6] *z + matrix[7],
		matrix[8]*x + matrix[9]*y + matrix[10]*z + matrix[11]
	)

-- Transforms a 3d vector via a 4x3 matrix
fun transformVector((x, y, z): double3, matrix: []f64): (f64, f64, f64) =
	(
		matrix[0]*x + matrix[1]*y + matrix[2] *z,
		matrix[4]*x + matrix[5]*y + matrix[6] *z,
		matrix[8]*x + matrix[9]*y + matrix[10]*z
	)

-- Transforms a 3d ray with origin and direction via a 4x3 matrix
fun transformRay((o, d, xy): ray, matrix: []f64): ray =
	let o = transformPoint(o, matrix)
	let d = normaliseVector(transformVector(d, matrix))
	in (o, d, xy)
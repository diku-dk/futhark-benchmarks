type double3 = (f64, f64, f64)

-- ---Basic vector arithmetic---
-- Adds two vectors together
fun addVectors((x1, y1, z1): double3, (x2, y2, z2): double3): double3      = (x1+x2, y1+y2, z1+z2)

-- Subtracts two vector b from vector a
fun subtractVectors((x1, y1, z1): double3, (x2, y2, z2): double3): double3 = (x1-x2, y1-y2, z1-z2)

-- Multiplies all vector components by a float. (scalar)
fun scalarVectorFloat((x, y, z): double3, f: f64): double3                 = (x*f, y*f, z*f)

-- Divides all vector components by a float
fun scalarDivVectorFloat((x, y, z): double3, f: f64): double3              = scalarVectorFloat((x, y, z), 1.0/f)

-- Divides vector a by vector b
fun divideVectors((x1, y1, z1): double3, (x2, y2, z2): double3): double3   = (x1/x2, y1/y2, z1/z2)

-- Adds two vectors together
fun (a: double3) +^ (b: double3): double3 = addVectors(a, b)

-- Subtracts two vector b from vector a
fun (a: double3) -^ (b: double3): double3 = subtractVectors(a, b)

-- Multiplies vector components by a float. (scalar)
fun (v: double3) *^ (f: f64): double3     = scalarVectorFloat(v, f)

-- Divides all components by a float.
fun (v: double3) /^ (f: f64): double3     = scalarDivVectorFloat(v, f)

-- Divides vector a by vector b
fun (a: double3) // (b: double3): double3 = divideVectors(a, b)

-- ---Advanced vector functions---
-- Calculates the length of a vector
fun magnitudeVector((x, y, z): double3): f64 =
	intrinsics.sqrt64 (x*x + y*y + z*z)

-- Makes a vector's length one
fun normaliseVector(v: double3): double3 =
	let mag = magnitudeVector(v)
	in if (mag == 0.0) 
	then (0.0, 0.0, 0.0)
	else v /^ mag

-- Calculates the cross product vector from two vectors
fun crossProductVector((ax: f64, ay: f64, az: f64), (bx: f64, by: f64, bz: f64)): double3 =
	(ay * bz - az * by, az * bx - ax * bz, ax * by - ay * bx)

-- Calculates the dot product from two vectors
fun dotProductVector((ax, ay, az): double3, (bx, by, bz): double3): f64 =
	(ax * bx + ay * by + az * bz)

-- Makes every component of a vector its absolute value
fun absVector((x, y, z): double3): double3 =
	(intrinsics.abs(x), intrinsics.abs(y), intrinsics.abs(z))
-- | A module type for standard Euclidean vectors.
--
-- Vectors of arbitrary dimension can be constructed with
-- `mk_vspace`@term by passing in a `vector`@mtype@"vector" and a
-- `real`@mtype@"/fublib/math" module, although for most uses the
-- `mk_vspace_2d`@term and `mk_vspace_3d`@term modules are simpler and
-- sufficient.

module type vspace = {
  type real

  -- | A vector type.  Semantically a sequence of `real`s.
  type vector

  val +: vector -> vector -> vector
  val -: vector -> vector -> vector

  -- | Inner product.
  val dot: vector -> vector -> real

  -- | Squared norm.
  val quadrance: vector -> real

  val scale: real -> vector -> vector
  val norm: vector -> real

  -- | Transform to unit vectortor.
  val normalise: vector -> vector
}

-- | A two-dimensional vector space is just a vector space, but we
-- give the vectors a convenient record type.
module type vspace_2d = {
  type real

  include vspace with real = real with vector = {x: real, y: real}
}

-- | Construct a 2D vector space.
module mk_vspace_2d(real: real): vspace_2d with real = real.t = {
  type real = real.t

  type vector = {x: real, y: real}

  let (a: vector) + (b: vector) =
    real.({x=a.x+b.x, y=a.y+b.y})

  let (a: vector) - (b: vector) =
    real.({x=a.x-b.x, y=a.y-b.y})

  let dot (a: vector) (b: vector) =
    real.(a.x*b.x + a.y*b.y)

  let quadrance v = dot v v

  let scale (s: real) ({x,y}: vector) =
    real.({x=x*s, y=y*s})

  let norm = quadrance >-> real.sqrt

  let normalise (v: vector): vector =
    let l = norm v
    in scale (real.i32 1 real./ l) v
}

-- | A three-dimensional vector space is just a vector space, but we
-- give the vectors a convenient record type.  Also, cross product is
-- defined.
module type vspace_3d = {
  type real

  include vspace with real = real with vector = {x: real, y: real, z: real}

  -- | Cross product.
  val cross: vector -> vector -> vector
}

-- | Construct a 3D vector space.
module mk_vspace_3d(real: real): vspace_3d with real = real.t = {
  type real = real.t

  type vector = {x: real, y: real, z: real}

  let (a: vector) + (b: vector) =
    real.({x=a.x+b.x, y=a.y+b.y, z=a.z+b.z})

  let (a: vector) - (b: vector) =
    real.({x=a.x-b.x, y=a.y-b.y, z=a.z-b.z})

  let dot (a: vector) (b: vector) =
    real.(a.x*b.x + a.y*b.y + a.z*b.z)

  let cross ({x=ax,y=ay,z=az}: vector)
            ({x=bx,y=by,z=bz}: vector): vector =
    real.({x=ay*bz-az*by, y=az*bx-ax*bz, z=ax*by-ay*bx})

  let quadrance v = dot v v

  let scale (s: real) ({x,y,z}: vector) =
    real.({x=x*s, y=y*s, z=z*s})

  let norm = quadrance >-> real.sqrt

  let normalise (v: vector): vector =
    let l = norm v
    in scale (real.i32 1 real./ l) v
}

import "vector"

-- | Construct an arbitrary-dimensional vector space.  The
-- dimensionality is given by the vector representation that is
-- provided.
module mk_vspace(V: vector) (real: real):
       vspace with real = real.t with vector = V.vector real.t = {
  type real = real.t
  type vector = V.vector real

  let a + b = V.map (uncurry (real.+)) (V.zip a b)

  let a - b = V.map (uncurry (real.-)) (V.zip a b)

  let dot (a: vector) (b: vector) =
    V.reduce (real.+) (real.i32 0) (V.map (uncurry (real.*)) (V.zip a b))

  let scale (s: real) = V.map (s real.*)

  let quadrance v = dot v v

  let norm = quadrance >-> real.sqrt

  let normalise (v: vector): vector =
    let l = norm v
    in scale (real.i32 1 real./ l) v
}

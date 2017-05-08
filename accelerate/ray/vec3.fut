import "/futlib/math"

default (f32)

type vec3 = (f32,f32,f32)

let ((x1,y1,z1): vec3) + ((x2,y2,z2): vec3) =
  (x1 f32.+ x2, y1 f32.+ y2, z1 f32.+ z2)

let ((x1,y1,z1): vec3) - ((x2,y2,z2): vec3) =
  (x1 f32.- x2, y1 f32.- y2, z1 f32.- z2)

let dot ((x1,y1,z1): vec3) ((x2,y2,z2): vec3) =
  (x1 f32.* x2) f32.+ (y1 f32.* y2) f32.+ (z1 f32.* z2)

let (s: f32) *^ ((x,y,z): vec3) =
  (x f32.* s, y f32.* s, z f32.* s)

let norm ((x,y,z): vec3): f32 =
  f32.sqrt (x f32.* x f32.+ y f32.* y f32.+ z f32.* z)

let normalise (v: vec3): vec3 =
  let l = norm v
  in (1./l) *^ v

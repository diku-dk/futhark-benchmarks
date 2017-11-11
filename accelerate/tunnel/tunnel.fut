-- Port of https://github.com/AccelerateHS/accelerate-examples/blob/master/examples/tunnel/Main.hs
--
-- Quite straightforward.  The Accelerate guys write very pretty code.
-- I don't know if my port is correct.  It does generate a tunnel, but
-- it might not look exactly like the one Accelerate makes.
--
-- ==
-- compiled input { 10f32 800  600 }
-- compiled input { 10f32 1000 1000 }
-- compiled input { 10f32 2000 2000 }
-- compiled input { 10f32 4000 4000 }
-- compiled input { 10f32 8000 8000 }

import "/futlib/math"
import "/futlib/array"

type v2 = (f32,f32)

let v2Add(x: v2, y: v2): v2 =
  (x.1 + y.1, x.2 + y.2)

let v2Sub(x: v2, y: v2): v2 =
  (x.1 - y.1, x.2 - y.2)

let norm(x: v2): f32 =
  f32.sqrt(x.1**2f32 + x.2**2f32)

-- Fractional part of a number.
let fract(x: f32): f32 =
  x - r32(t32(x))

let clamp(lower: f32, x: f32, upper: f32): f32 =
  if x < lower then lower
  else if x > upper then upper
  else x

let smoothstep(edge0: f32, edge1: f32, x: f32): f32 =
  let t = clamp(0f32, ((x-edge0) / (edge1-edge0)), 1.0f32)
  in t*t*(3f32 - 2f32*t)

let dot(p0: v2, p1: v2): f32 =
  p0.1 * p1.1 + p0.2 * p1.2

let rand2(p: v2): v2 =
  let x = (127.1f32, 311.7f32)
  let y = (269.5f32, 183.3f32)
  in (fract(f32.sin(dot(p,x)) * 43758.5453f32),
      fract(f32.sin(dot(p,y)) * 43758.5453f32))

let rand1(p: v2): f32 =
  let z = (419.2f32, 371.9f32)
  in fract(f32.sin(dot(p,z)) * 833458.57832f32)

let sample(irregular: f32, cell: v2, cellOffset: v2, sharpness: f32, i: i32, j: i32): v2 =
  let samplePos = (r32(i), r32(j))
  let centre = (let u = rand2(v2Add(cell, samplePos))
                in (u.1 * irregular, u.2 * irregular))
  let centreDist = norm(v2Add(v2Sub(samplePos, cellOffset), centre))
  let det = (1f32 - smoothstep(0f32, 1.414f32, centreDist)) ** sharpness
  let colour = rand1(v2Add(cell, samplePos))
  in (colour * det, det)

let voronoise(xy: v2, irregular: f32, smoothness: f32): f32 =
  let cell = (r32(t32(xy.1)), r32(t32(xy.2)))
  let cellOffset = (fract(xy.1), fract(xy.2))
  let sharpness = 1f32 + 63f32 * ((1f32-smoothness) ** 4f32)
  let samples = loop samples = (0f32,0f32) for i in [-2...2] do
    (loop samples for j in [-2...2] do
     v2Add(samples, sample(irregular, cell, cellOffset, sharpness, i, j)))
  in samples.1 / samples.2

let mod'(n: f32, d: f32): f32 =
  n - r32(t32(n/d)) * d

let rgb(r: f32, g: f32, b: f32): i32 =
  (t32(r*255f32)&0xFF) << 16 |
  (t32(g*255f32)&0xFF) << 8 |
  (t32(b*255f32)&0xFF)

let tunnel(time: f32) (x: i32) (y: i32): i32 =
  let pt2 = (1.2f32 * r32(x), 1.2f32 * r32(y))
  let rInv = 1.0f32 / norm(pt2)
  let pt3 = v2Sub((pt2.1 * rInv, pt2.2 * rInv),
                  (rInv + 2f32 * mod'(time, 6000f32), 0f32))
  let c1 = (0.659f32, 0.772f32, 1f32)
  let c2 = (let x = voronoise((5f32*pt3.1, 5f32*pt3.2), 1f32, 1f32) + 0.240f32*rInv
            in (c1.1 * x, c1.2 * x, c1.3 * x))
  in rgb(c2.1, c2.2, c2.3)

let main(time: f32, w: i32, h: i32): [w][h]i32 =
  map (\(x: i32): [h]i32  ->
        map (tunnel time x) (map (-(h/2)) (iota(h)))) (
      map (-(w/2)) (iota(w)))

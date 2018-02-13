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
import "/futlib/vec2"
import "/futlib/colour"

default (f32)

module vec2 = mk_vec2 f32

type v2 = (f32,f32)

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

let rand2(p: vec2.vec): vec2.vec =
  let x = {x=127.1f32, y=311.7f32}
  let y = {x=269.5f32, y=183.3f32}
  in {x= fract(f32.sin(vec2.dot p x) * 43758.5453f32),
      y= fract(f32.sin(vec2.dot p y) * 43758.5453f32)}

let rand1(p: vec2.vec): f32 =
  let z = {x=419.2f32, y=371.9f32}
  in fract(f32.sin(vec2.dot p z) * 833458.57832f32)

let sample(irregular: f32, cell: vec2.vec, cellOffset: vec2.vec, sharpness: f32, i: i32, j: i32): vec2.vec =
  let samplePos = {x=r32 i, y=r32 j}
  let u = rand2(vec2.(cell + samplePos))
  let centre = {x=u.x * irregular, y=u.y * irregular}
  let centreDist = vec2.norm(vec2.(samplePos - cellOffset + centre))
  let det = (1f32 - smoothstep(0f32, 1.414f32, centreDist)) ** sharpness
  let colour = rand1(vec2.(cell + samplePos))
  in {x=colour * det, y=det}

let voronoise(xy: vec2.vec, irregular: f32, smoothness: f32): f32 =
  let cell = {x=r32(t32 xy.x), y=r32(t32 xy.y)}
  let cellOffset = {x=fract xy.x, y=fract xy.y}
  let sharpness = 1f32 + 63f32 * ((1f32-smoothness) ** 4f32)
  let samples = loop samples = {x=0.0, y=0.0} for i in -2...2 do
    (loop samples for j in -2...2 do
     vec2.(samples + sample(irregular, cell, cellOffset, sharpness, i, j)))
  in samples.x / samples.y

let mod'(n: f32, d: f32): f32 =
  n - r32(t32(n/d)) * d

let tunnel(time: f32) (x: i32) (y: i32): argb.colour =
  let pt2 = {x=1.2 * r32 x, y=1.2 * r32 y}
  let rInv = 1.0f32 / vec2.norm pt2
  let pt3 = {x=pt2.x * rInv, y=pt2.y * rInv} vec2.-
            {x=rInv + 2.0 * mod'(time, 6000.0), y=0.0}
  let c1 = (0.659f32, 0.772f32, 1f32)
  let x = voronoise({x=5.0*pt3.x, y=5.0*pt3.y}, 1.0, 1.0) + 0.240*rInv
  in argb.from_rgba (c1.1 * x) (c1.2 * x) (c1.3 * x) 1.0

let main(time: f32, w: i32, h: i32): [w][h]i32 =
  map (\(x: i32): [h]i32  ->
       map (tunnel time x) (map (\x -> x-(h/2)) (iota(h)))) (
      map (\x -> x-(w/2)) (iota(w)))

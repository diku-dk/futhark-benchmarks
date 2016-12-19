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

type v2 = (f32,f32)

fun v2Add(x: v2, y: v2): v2 =
  (#0 x + #0 y, #1 x + #1 y)

fun v2Sub(x: v2, y: v2): v2 =
  (#0 x - #0 y, #1 x - #1 y)

fun norm(x: v2): f32 =
  sqrt32(#0 x**2f32 + #1 x**2f32)

-- Fractional part of a number.
fun fract(x: f32): f32 =
  x - f32(int(x))

fun clamp(lower: f32, x: f32, upper: f32): f32 =
  if x < lower then lower
  else if x > upper then upper
  else x

fun smoothstep(edge0: f32, edge1: f32, x: f32): f32 =
  let t = clamp(0f32, ((x-edge0) / (edge1-edge0)), 1.0f32)
  in t*t*(3f32 - 2f32*t)

fun dot(p0: v2, p1: v2): f32 =
  #0 p0 * #0 p1 + #1 p0 * #1 p1

fun rand2(p: v2): v2 =
  let x = (127.1f32, 311.7f32)
  let y = (269.5f32, 183.3f32)
  let q = (dot(p,x), dot(p,y))
  in (fract(sin32(#0 q) * 43758.5453f32),
      fract(sin32(#1 q) * 43758.5453f32))

fun rand1(p: v2): f32 =
  let z = (419.2f32, 371.9f32)
  in fract(sin32(dot(p,z)) * 833458.57832f32)

fun voronoise(xy: v2, irregular: f32, smoothness: f32): f32 =
  let cell = (f32(int(#0 xy)), f32(int(#1 xy)))
  let cellOffset = (fract(#0 xy), fract(#1 xy))
  let sharpness = 1f32 + 63f32 * ((1f32-smoothness) ** 4f32)
  loop (samples = (0f32,0f32)) = for (-2) <= i < 3 do
    (loop (samples) = for (-2) <= j < 3 do
     v2Add(samples, sample(irregular, cell, cellOffset, sharpness, i, j))
     in samples)
  in #0 samples / #1 samples

fun sample(irregular: f32, cell: v2, cellOffset: v2, sharpness: f32, i: int, j: int): v2 =
  let samplePos = (f32(i), f32(j))
  let centre = (let u = rand2(v2Add(cell, samplePos))
                in (#0 u * irregular, #1 u * irregular))
  let centreDist = norm(v2Add(v2Sub(samplePos, cellOffset), centre))
  let det = (1f32 - smoothstep(0f32, 1.414f32, centreDist)) ** sharpness
  let colour = rand1(v2Add(cell, samplePos))
  in (colour * det, det)

fun mod'(n: f32, d: f32): f32 =
  n - f32(int(n/d)) * d

fun tunnel(time: f32) (x: int) (y: int): int =
  let pt2 = (1.2f32 * f32(x), 1.2f32 * f32(y))
  let rInv = 1.0f32 / norm(pt2)
  let pt3 = v2Sub((#0 pt2 * rInv, #1 pt2 * rInv),
                  (rInv + 2f32 * mod'(time, 6000f32), 0f32))
  let c1 = (0.659f32, 0.772f32, 1f32)
  let c2 = (let x = voronoise((5f32*#0 pt3, 5f32*#1 pt3), 1f32, 1f32) + 0.240f32*rInv
            in (#0 c1 * x, #1 c1 * x, #2 c1 * x))
  in rgb(#0 c2, #1 c2, #2 c2)

fun rgb(r: f32, g: f32, b: f32): int =
  (int(r*255f32)&0xFF) << 16 |
  (int(g*255f32)&0xFF) << 8 |
  (int(b*255f32)&0xFF)

fun main(time: f32, w: int, h: int): [w][h]int =
  map (fn (x: int): [h]int  =>
        map (tunnel time x) (map (-(h/2)) (iota(h)))) (
      map (-(w/2)) (iota(w)))

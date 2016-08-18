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

fun v2 v2Add(v2 x, v2 y) =
  (x.0 + y.0, x.1 + y.1)

fun v2 v2Sub(v2 x, v2 y) =
  (x.0 - y.0, x.1 - y.1)

fun f32 norm(v2 x) =
  sqrt32(x.0**2f32 + x.1**2f32)

-- Fractional part of a number.
fun f32 fract(f32 x) =
  x - f32(int(x))

fun f32 clamp(f32 lower, f32 x, f32 upper) =
  if x < lower then lower
  else if x > upper then upper
  else x

fun f32 smoothstep(f32 edge0, f32 edge1, f32 x) =
  let t = clamp(0f32, ((x-edge0) / (edge1-edge0)), 1.0f32)
  in t*t*(3f32 - 2f32*t)

fun f32 dot(v2 p0, v2 p1) =
  p0.0 * p1.0 + p0.1 * p1.1

fun v2 rand2(v2 p) =
  let x = (127.1f32, 311.7f32)
  let y = (269.5f32, 183.3f32)
  let q = (dot(p,x), dot(p,y))
  in (fract(sin32(q.0) * 43758.5453f32),
      fract(sin32(q.1) * 43758.5453f32))

fun f32 rand1(v2 p) =
  let z = (419.2f32, 371.9f32)
  in fract(sin32(dot(p,z)) * 833458.57832f32)

fun f32 voronoise(v2 xy, f32 irregular, f32 smoothness) =
  let cell = (f32(int(xy.0)), f32(int(xy.1)))
  let cellOffset = (fract(xy.0), fract(xy.1))
  let sharpness = 1f32 + 63f32 * ((1f32-smoothness) ** 4f32)
  loop (samples = (0f32,0f32)) = for -2 <= i < 3 do
    (loop (samples) = for -2 <= j < 3 do
     v2Add(samples, sample(irregular, cell, cellOffset, sharpness, i, j))
     in samples)
  in samples.0 / samples.1

fun v2 sample(f32 irregular, v2 cell, v2 cellOffset, f32 sharpness, int i, int j) =
  let samplePos = (f32(i), f32(j))
  let centre = (let u = rand2(v2Add(cell, samplePos))
                in (u.0 * irregular, u.1 * irregular))
  let centreDist = norm(v2Add(v2Sub(samplePos, cellOffset), centre))
  let det = (1f32 - smoothstep(0f32, 1.414f32, centreDist)) ** sharpness
  let colour = rand1(v2Add(cell, samplePos))
  in (colour * det, det)

fun f32 mod'(f32 n, f32 d) =
  n - f32(int(n/d)) * d

fun int tunnel(f32 time, int x, int y) =
  let pt2 = (1.2f32 * f32(x), 1.2f32 * f32(y))
  let rInv = 1.0f32 / norm(pt2)
  let pt3 = v2Sub((pt2.0 * rInv, pt2.1 * rInv),
                  (rInv + 2f32 * mod'(time, 6000f32), 0f32))
  let c1 = (0.659f32, 0.772f32, 1f32)
  let c2 = (let x = voronoise((5f32*pt3.0, 5f32*pt3.1), 1f32, 1f32) + 0.240f32*rInv
            in (c1.0 * x, c1.1 * x, c1.2 * x))
  in rgb(c2.0, c2.1, c2.2, 0f32)

fun int rgb(f32 r, f32 g, f32 b, f32 a) =
  (int(r*255f32)&0xFF) << 16 |
  (int(g*255f32)&0xFF) << 8 |
  (int(b*255f32)&0xFF)

fun [w][h]int main(f32 time, int w, int h) =
  map(fn [h]int (int x) =>
        map(tunnel(time, x), map(-(h/2), iota(h))),
      map(-(w/2), iota(w)))

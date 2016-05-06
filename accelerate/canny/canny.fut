-- Port of the canny example from Accelerate:
-- https://github.com/AccelerateHS/accelerate-examples/blob/master/examples/canny/src-acc
--
-- Only implements the first seven[sic] data-parallel stages, not the final
-- recursive algorithm (which in the Accelerate implementation is also
-- done in Repa).
--
-- ==

-- compiled input {
--  [[1038851507, 1834550796, 2046247850],
--   [1709637579, -- 2011955188, -2110545476],
--   [-902148259, 643549096, -787906599],
--   [1283118839, -1359985963, 733366311],
--   [331229149, 824288910, -112514126]]
-- }
-- output {
--   [0i32, 2i32, 10i32, 11i32]
-- }

default (f32)

fun int min(int x, int y) =
  if x < y then x else y

fun f32 pi() = 3.14159265359

fun f32 luminanceOfRGBA32(i32 p) =
  let r = i8(p >> 24)
  let g = i8(p >> 16)
  let b = i8(p >> 8)
  let r' = 0.3  * f32(r)
  let g' = 0.59 * f32(g)
  let b' = 0.11 * f32(b)
  in (r' + g' + b') / 255.0

fun int clamp(int lower, int x, int upper) =
  if x < lower then lower
  else if x > upper then upper
  else x

fun int orientUndef() = 0
fun int orientPosD() = 64
fun int orientVert() = 128
fun int orientNegD() = 192
fun int orientHoriz() = 255

fun f32 edgeNone() = 0.0
fun f32 edgeWeak() = 0.5
fun f32 edgeStrong() = 1.0

fun [[f32,w],h] toGreyscale([[i32,w],h] img) =
  map(fn [f32,w] ([i32,w] row) =>
        map(255.0*, map(luminanceOfRGBA32, row)),
      img)

fun [[f32,w],h] gaussianX([[f32,w],h] img) =
  unsafe
  map(fn [f32,w] (int y) =>
        map(fn f32 (int x) =>
              let a = img[clamp(0,x-2,w-1),y] * (1.0 / 16.0)
              let b = img[clamp(0,x-1,w-1),y] * (4.0 / 16.0)
              let c = img[clamp(0,x+0,w-1),y] * (6.0 / 16.0)
              let d = img[clamp(0,x+1,w-1),y] * (4.0 / 16.0)
              let e = img[clamp(0,x+2,w-1),y] * (1.0 / 16.0)
              in a + b + c + d + e,
            iota(w)),
      iota(h))

fun [[f32,w],h] gaussianY([[f32,w],h] img) =
  unsafe
  map(fn [f32,w] (int y) =>
        map(fn f32 (int x) =>
              unsafe
              let a = img[x,clamp(0,y-2,h-1)] * (1.0 / 16.0)
              let b = img[x,clamp(0,y-1,h-1)] * (4.0 / 16.0)
              let c = img[x,clamp(0,y+0,h-1)] * (6.0 / 16.0)
              let d = img[x,clamp(0,y+1,h-1)] * (4.0 / 16.0)
              let e = img[x,clamp(0,y+2,h-1)] * (1.0 / 16.0)
              in a + b + c + d + e,
            iota(w)),
      iota(h))

fun [[(f32,int),w],h] gradiantMagDir(f32 low, [[f32,w],h] img) =
  unsafe
  map(fn [(f32,int),w] (int y) =>
        map(fn (f32,int) (int x) =>
              unsafe
              let v0 = img[clamp(0, x-1, w-1), clamp(0, y-1, h-1)]
              let v1 = img[clamp(0, x+0, w-1), clamp(0, y-1, h-1)]
              let v2 = img[clamp(0, x+1, w-1), clamp(0, y-1, h-1)]

              let v3 = img[clamp(0, x-1, w-1), clamp(0, y+0, h-1)]
              let v4 = img[clamp(0, x+1, w-1), clamp(0, y+0, h-1)]

              let v5 = img[clamp(0, x-1, w-1), clamp(0, y+1, h-1)]
              let v6 = img[clamp(0, x+0, w-1), clamp(0, y+1, h-1)]
              let v7 = img[clamp(0, x+1, w-1), clamp(0, y+1, h-1)]

              let dx = v2 + (2.0*v4) + v7 - v0 - (2.0*v3) - v5
              let dy = v0 + (2.0*v1) + v2 - v5 - (2.0*v6) - v7

              let mag = sqrt32(dx * dx + dy * dy)

              let theta = atan2_32(dy, dx)
              let alpha = (theta - (pi()/8.0)) * (4.0/pi())

              -- Normalise the angle to between [0..8)
              let norm = alpha + if alpha <= 0.0 then 8.0 else 0.0

              let dir = if abs(dx) <= low && abs(dy) <= low
                        then 0
                        else min(64 * (1 + i32(norm) % 4), 255)

              in (mag, dir),
            iota(w)),
      iota(h))

fun [[f32,w],h] nonMaximumSuppression(f32 low, f32 high, [[(f32,int),w],h] magdir) =
  unsafe
  map(fn [f32,w] (int y) =>
        map(fn f32 (int x) =>
              let (mag, dir) = magdir[x,y]
              let offsetx = if dir > orientVert() then -1
                            else if dir < orientVert() then 1
                            else 0
              let offsety = if dir < orientHoriz() then -1 else 0
              let (fwd, _) = magdir[clamp(0, x+offsetx, w-1),
                                    clamp(0, y+offsety, h-1)]
              let (rev, _) = magdir[clamp(0, x-offsetx, w-1),
                                    clamp(0, y-offsety, h-1)]

              in if dir == orientUndef() || mag < low || mag < fwd || mag < rev
                 then 0.0
                 else if mag >= high then 1.0 else 0.5,
         iota(w)),
      iota(h))

fun [int] selectStrong([[f32,w],h] img) =
  let strong = map(fn int (f32 x) =>
                     if x == edgeStrong() then 1 else 0,
                   reshape((w*h), img))
  -- The original Accelerate implementation used an exclusive scan
  -- here, so we have to play with the indices.
  let targetIdxAndLen = scan(+, 0, strong)
  let (targetIdx, len') = split((size(0,strong)-1), targetIdxAndLen)
  let len = len'[0]
  let indices = iota(w*h)
  let zeros = replicate(len, 0)
  let (indices', values) =
    unzip(map(fn (int, i32) (int i) =>
                if unsafe strong[i+1] == 0
                then (-1, 0)
                else (targetIdx[i], i+1),
              iota(w*h-1)))
  in write(indices', values, zeros)

fun [i32] main(f32 low, f32 high, [[i32,w],h] img) =
  selectStrong
  (nonMaximumSuppression
   (low, high, gradiantMagDir
    (low, gaussianY
     (gaussianX
      (toGreyscale(img))))))

-- | A port of Conal Elliott's Pan library from "Functional Images".

-- | Point in the plane
type point = (f32,f32)

-- | Generic image
type img 'a = point -> a

-- | Region (b/w)
type region = img bool

-- | Floats in [0;1]
type frac = f32

-- | Red,green,blue,alpha
type fcolor = (frac,frac,frac,frac)

-- | Color images
type cimage = img fcolor

type polar_point = (f32, f32)

-- | Transformations
type transform = point -> point

type vector = (point, point)

let vstrip : region =
  \(x,_) -> f32.abs x <= 0.5f32

let even x = x % 2i32 == 0i32

let floori = i32.f32

let checker : region =
  \(x,y) -> even(floori x + floori y)

let distO (x,y) = f32.sqrt(x*x+y*y)

let altRings : region =
  distO >-> floori >-> even

let pi = f32.pi
let fromPolar (r,t) = (r*f32.cos t, r*f32.sin t)
let toPolar (x,y) = (distO (x,y), f32.atan2 y x)

let polarChecker n : region =
  let sc (r,t) = (r, t * r32 n / pi)
  in toPolar >-> sc >-> checker

let wavDist : img frac =
 \ p -> (1f32 + f32.cos (pi * distO p)) / 2f32

let boolToFColor b =
  if b then (0f32,0f32,0f32,1f32)
  else (1f32,1f32,1f32,1f32)

let fracToFColor (f:frac) = (f,f,f,1f32)

let lerpC w ((a1,r1,g1,b1):fcolor) ((a2,r2,g2,b2):fcolor) : fcolor =
  let h x1 x2 = w * x1 + (1f32-w)*x2
  in (h a1 a2, h r1 r2, h g1 g2, h b1 b2)

let bilerpC w h c1 c2 c3 c4 =
  let c_low = lerpC w c1 c2
  let c_high = lerpC w c3 c4
  in lerpC h c_low c_high

let overC ((a1,r1,g1,b1):fcolor) ((a2,r2,g2,b2):fcolor) : fcolor =
  let h x1 x2 = x1 + (1f32-a1)*x2
  in (h a1 a2,h r1 r2,h g1 g2, h b1 b2)

let lift1 h f1 p = h (f1 p)
let lift2 h f1 f2 p = h (f1 p) (f2 p)
let lift3 h f1 f2 f3 p = h (f1 p) (f2 p) (f3 p)

let over : cimage -> cimage -> cimage =
  \ x y -> lift2 overC x y

let cond 'a : region -> img a -> img a -> img a =
  \ x y c -> lift3 (\ a b c -> if a then b else c) x y c

let lerpI : img frac -> cimage -> cimage -> cimage =
  \ x y z -> lift3 lerpC x y z

let blueI = const (0f32,0f32,1f32,1f32)
let redI = const (1f32,0f32,0f32,1f32)
let greenI = const (0f32,1f32,0f32,1f32)
let yellowI = const (0f32,1f32,1f32,1f32)

let rbRings = lerpI wavDist redI blueI

let mystique : cimage =
  lerpI (const 0.2f32) (checker >-> boolToFColor) rbRings

let (<<|>>) : f32 -> f32 -> bool = \(x:f32) y -> f32.abs(x-y) < 0.05f32

let f : region = \(x,y) -> y <<|>> (x*x*x - 2f32*x*x + 1.5f32)

let (<||>) : region -> region -> region =
  \ r1 r2 p -> r1 p || r2 p

let coord : region =
  \(x,y) -> x <<|>> 0f32 || y <<|>> 0f32

let circ : region =                          -- $1 = x^2 + y^2$
  \(x,y) -> 1f32 <<|>> (x*x + y*y)

let invY 'a (f : img a) : img a =
  \(x,y) -> f (x,-y)

let translateP ((dx,dy): point): transform =
  \(x,y) -> (x+dx, y+dy)

let scaleP ((sx,sy): point): transform =
  \(x,y) -> (sx*x, sy*y)

let uscaleP (s: f32): transform = scaleP (s,s)

let rotateP (theta: f32): transform =
  \(x,y) -> (x * f32.cos(theta) - y * f32.sin(theta),
             y * f32.cos(theta) + x * f32.sin(theta))

let udisk: region =
  \p -> distO p < 1f32

type filter 'a = img a -> img a

let translate 'a ((dx, dy): point): filter a =
  (translateP (-dx, -dy) >->)

let scale 'a ((sx, sy): point): filter a =
  (scaleP (1f32/sx, 1f32/sy) >->)

let uscale 'a (s: f32): filter a =
  (uscaleP (1f32/s) >->)

let rotate' 'a (theta: f32): filter a =
  (rotateP (-theta) >->)

let swirlP (r: f32): transform =
  \p -> rotateP (distO p * 2f32 * f32.pi / r) p

let swirl 'a (r: f32): filter a =
  (swirlP (-r) >->)

type filterC = filter fcolor

type time = f32
type anim 'a = time -> img a

let universeR: region = const true
let emptyR: region = const false
let compR: region -> region = lift1 (!)
let intersectR: region -> region -> region = lift2 (&&)
let unionR: region -> region -> region = lift2 (||)
let xorR: region -> region -> region = lift2 (\x y -> bool.i32 (i32.bool x ^ i32.bool y))
let subtractR: region -> region -> region = \r r' -> r `intersectR` compR r'

let annulus (inner: frac): region = udisk `subtractR` uscale inner udisk

let radReg n: region =
  let test (_, theta) = even (i32.f32 (theta * r32 n / f32.pi))
  in toPolar >-> test

let wedgeAnnulus inner n: region = annulus inner `intersectR` radReg n

let shiftXor r: filter bool =
  \reg -> let reg' d = translate (d,0f32) reg
          in reg' r `xorR` reg' (-r)

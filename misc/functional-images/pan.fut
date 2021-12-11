-- | A port of Conal Elliott's Pan library from "Functional Images".

-- | Point in the plane
type point = (f32,f32)

-- | Generic image
type^ img 'a = point -> a

-- | Region (b/w)
type^ region = img bool

-- | Floats in [0;1]
type frac = f32

-- | Red,green,blue,alpha
type fcolor = (frac,frac,frac,frac)

-- | Color images
type^ cimage = img fcolor

type polar_point = (f32, f32)

-- | Transformations
type^ transform = point -> point

type vector = (point, point)

def vstrip : region =
  \(x,_) -> f32.abs x <= 0.5f32

def even x = x % 2i32 == 0i32

def floori = i32.f32

def checker : region =
  \(x,y) -> even(floori x + floori y)

def distO (x,y) = f32.sqrt(x*x+y*y)

def altRings : region =
  distO >-> floori >-> even

def pi = f32.pi
def fromPolar (r,t) = (r*f32.cos t, r*f32.sin t)
def toPolar (x,y) = (distO (x,y), f32.atan2 y x)

def polarChecker n : region =
  let sc (r,t) = (r, t * r32 n / pi)
  in toPolar >-> sc >-> checker

def wavDist : img frac =
 \ p -> (1f32 + f32.cos (pi * distO p)) / 2f32

def boolToFColor b =
  if b then (0f32,0f32,0f32,1f32)
  else (1f32,1f32,1f32,1f32)

def fracToFColor (f:frac) = (f,f,f,1f32)

def lerpC w ((a1,r1,g1,b1):fcolor) ((a2,r2,g2,b2):fcolor) : fcolor =
  let h x1 x2 = w * x1 + (1f32-w)*x2
  in (h a1 a2, h r1 r2, h g1 g2, h b1 b2)

def bilerpC w h c1 c2 c3 c4 =
  let c_low = lerpC w c1 c2
  let c_high = lerpC w c3 c4
  in lerpC h c_low c_high

def overC ((a1,r1,g1,b1):fcolor) ((a2,r2,g2,b2):fcolor) : fcolor =
  let h x1 x2 = x1 + (1f32-a1)*x2
  in (h a1 a2,h r1 r2,h g1 g2, h b1 b2)

def lift1 h f1 p = h (f1 p)
def lift2 h f1 f2 p = h (f1 p) (f2 p)
def lift3 h f1 f2 f3 p = h (f1 p) (f2 p) (f3 p)

def over : cimage -> cimage -> cimage =
  \ x y -> lift2 overC x y

def cond 'a : region -> img a -> img a -> img a =
  \ x y c -> lift3 (\ a b c -> if a then b else c) x y c

def lerpI : img frac -> cimage -> cimage -> cimage =
  \ x y z -> lift3 lerpC x y z

def blueI = const (0f32,0f32,1f32,1f32)
def redI = const (1f32,0f32,0f32,1f32)
def greenI = const (0f32,1f32,0f32,1f32)
def yellowI = const (0f32,1f32,1f32,1f32)

def rbRings = lerpI wavDist redI blueI

def mystique : cimage =
  lerpI (const 0.2f32) (checker >-> boolToFColor) rbRings

def (<<|>>) : f32 -> f32 -> bool = \(x:f32) y -> f32.abs(x-y) < 0.05f32

def f : region = \(x,y) -> y <<|>> (x*x*x - 2f32*x*x + 1.5f32)

def (<||>) : region -> region -> region =
  \ r1 r2 p -> r1 p || r2 p

def coord : region =
  \(x,y) -> x <<|>> 0f32 || y <<|>> 0f32

def circ : region =                          -- $1 = x^2 + y^2$
  \(x,y) -> 1f32 <<|>> (x*x + y*y)

def invY 'a (f : img a) : img a =
  \(x,y) -> f (x,-y)

def translateP ((dx,dy): point): transform =
  \(x,y) -> (x+dx, y+dy)

def scaleP ((sx,sy): point): transform =
  \(x,y) -> (sx*x, sy*y)

def uscaleP (s: f32): transform = scaleP (s,s)

def rotateP (theta: f32): transform =
  \(x,y) -> (x * f32.cos(theta) - y * f32.sin(theta),
             y * f32.cos(theta) + x * f32.sin(theta))

def udisk: region =
  \p -> distO p < 1f32

type^ filter 'a = img a -> img a

def translate 'a ((dx, dy): point): filter a =
  (translateP (-dx, -dy) >->)

def scale 'a ((sx, sy): point): filter a =
  (scaleP (1f32/sx, 1f32/sy) >->)

def uscale 'a (s: f32): filter a =
  (uscaleP (1f32/s) >->)

def rotate' 'a (theta: f32): filter a =
  (rotateP (-theta) >->)

def swirlP (r: f32): transform =
  \p -> rotateP (distO p * 2f32 * f32.pi / r) p

def swirl 'a (r: f32): filter a =
  (swirlP (-r) >->)

type^ filterC = filter fcolor

type time = f32
type^ anim 'a = time -> img a

def universeR: region = const true
def emptyR: region = const false
def compR: region -> region = lift1 (\x -> !x)
def intersectR: region -> region -> region = lift2 (&&)
def unionR: region -> region -> region = lift2 (||)
def xorR: region -> region -> region = lift2 (\x y -> bool.i32 (i32.bool x ^ i32.bool y))
def subtractR: region -> region -> region = \r r' -> r `intersectR` compR r'

def annulus (inner: frac): region = udisk `subtractR` uscale inner udisk

def radReg n: region =
  let test (_, theta) = even (i32.f32 (theta * r32 n / f32.pi))
  in toPolar >-> test

def wedgeAnnulus inner n: region = annulus inner `intersectR` radReg n

def shiftXor r: filter bool =
  \reg -> let reg' d = translate (d,0f32) reg
          in reg' r `xorR` reg' (-r)

import "/futlib/colour"

type point = (f32,f32)               -- Point in the plane
type img 'a = point -> a             -- Generic image

type region = img bool               -- Region (b/w)

type frac = f32                      -- Floats in [0;1]
type fcolor = (frac,frac,frac,frac)  -- Red,green,blue,alpha

type cimage = img fcolor             -- Color images

let vstrip : region =
  \ (x,_) -> f32.abs x <= 0.5f32

let even x = x % 2i32 == 0i32

let floori = i32.f32

let checker : region =
  \(x,y) -> even(floori x + floori y)

let distO (x,y) = f32.sqrt(x*x+y*y)

let altRings : region =
  even <<| floori <<| distO

type polar_point = (f32, f32)

let pi = f32.pi
let fromPolar (r,t) = (r*f32.cos t, r*f32.sin t)
let toPolar (x,y) = (distO (x,y), f32.atan2 y x)

let polarChecker n : region =
  let sc (r,t) = (r, t * r32 n / pi)
  in checker <<| sc <<| toPolar

let wavDist : img frac =
 \ p -> (1f32 + f32.cos (pi * distO p)) / 2f32

let boolToFColor b =
  if b then (0f32,0f32,0f32,1f32)
  else (1f32,1f32,1f32,1f32)

let fracToFColor (f:frac) = (f,f,f,1f32)

let fcolorToColour (r,g,b,a) = argb.from_rgba r g b a

let boolToColour = fcolorToColour <<| boolToFColor

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
  lerpI (const 0.2f32) (boolToFColor <<| checker) rbRings

let (<<|>>) : f32 -> f32 -> bool = \ (x:f32) y -> f32.abs(x-y) < 0.05f32

let f : region = \ (x,y) -> y <<|>> (x*x*x - 2f32*x*x + 1.5f32)

let (<||>) : region -> region -> region =
  \ r1 r2 p -> r1 p || r2 p

let coord : region =
  \ (x,y) -> x <<|>> 0f32 || y <<|>> 0f32

let circ : region =                          -- $1 = x^2 + y^2$
  \ (x,y) -> 1f32 <<|>> (x*x + y*y)

let invY 'a (f : img a) : img a =
  \ (x,y) -> f (x,-y)


type image = { width: i32,
               height: i32,
               pixel: f32 -> (i32, i32) -> argb.colour
             }

let scale t =
  5f32*f32.cos(pi*(2f32 * t - r32(2i32 * t32 t)))

let transl (w:i32,h:i32) = \ (x,y) -> (x-w,y-h)

let toAnim (f:f32) (g:cimage) : image =
  let w = 200i32
  let h = 200i32
  in { width = w, height = h,
       pixel = \ t -> (fcolorToColour <<| g
                       <<| (\ (x,y) -> (scale (f*t) * r32 x/r32 w,
                                       scale (f*t) * r32 y/r32 h))
                       <<| transl (100i32,100i32))
     }

type colour_at_time = f32 -> argb.colour

let always: argb.colour -> colour_at_time = const

let cycle (r: f32) (c1: argb.colour) (c2: argb.colour): colour_at_time =
  \t -> let t' = (t - f32.trunc t) * 2f32 * r
        in if t' > 1.0f32 * r
           then argb.mix (r-t'+1f32) c1 (t'-1f32)  c2
           else argb.mix t' c1 (r-t') c2

let circle (r: f32) (c1: colour_at_time) (c2: colour_at_time): image =
  let width = t32(r*2f32)
  let height = t32(r*2f32)
  let pixel t (x, y) =
    if f32.sqrt((r-r32 x)**2f32 + (r-r32 y)**2f32) < r
    then c1 t
    else c2 t
  in {width, height, pixel}

let above (top: image) (below: image): image =
  let width = i32.max top.width below.width
  let height = top.height + below.height
  let pixel t (x, y) =
    if y < top.height
    then top.pixel t (x, y)
    else below.pixel t (x, y-top.height)
  in {width, height, pixel}

let beside (left: image) (right: image): image =
  let width = left.width + right.width
  let height = i32.max left.height right.height
  let pixel t (x, y) =
    if x < left.width
    then left.pixel t (x, y)
    else right.pixel t (x-left.width, y)
  in {width, height, pixel}

let invert ({width, height, pixel=orig_pixel}: image): image =
  let pixel t p = ~(orig_pixel t p)
  in {width, height, pixel}

let speedup (f: f32) ({width, height, pixel=orig_pixel}: image): image =
  let pixel t p = orig_pixel (t*f) p
  in {width, height, pixel}

let test_image =
  let stack = circle 100f32 (always argb.white) (cycle 1f32 argb.black argb.red) `above`
              circle 80f32 (cycle 1f32 argb.black argb.red) (always argb.white)
  in stack `beside` speedup 10f32 (invert stack) `above` (toAnim 1f32 mystique)
     `beside` (toAnim 0.2f32 rbRings)
     `beside` (toAnim 0.1f32 (boolToFColor <<| polarChecker 5))
     `above` ((toAnim 0.3f32 (boolToFColor <<| (coord <||> circ)))
              `beside` (toAnim 0.3f32 (invY(boolToFColor <<| f)))
              `beside` (toAnim 0.2f32 (boolToFColor <<| checker)))

entry test_image_render (t: f32) =
  let {width, height, pixel} = test_image
  in (width, height, tabulate_2d width height (curry (pixel t)))

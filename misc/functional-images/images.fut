import "/futlib/colour"
import "pan"

type image = { aspect: f32, -- width/height ratio.
               pixel: f32 -> point -> argb.colour
             }

let scale t =
  10f32*f32.cos(pi*(2f32 * t - r32(2i32 * t32 t)))

let transl (w:f32,h:f32) = \ (x,y) -> (x-w,y-h)

let fcolorToColour (r,g,b,a) = argb.from_rgba r g b a

let boolToColour = fcolorToColour <<| boolToFColor

let toAnim (f:f32) (g:cimage) : image =
  { aspect = 1f32,
    pixel = \t -> (fcolorToColour <<| g
                   <<| (\(x,y) -> (scale (f*t) * x,
                                   scale (f*t) * y)))
  }

type colour_at_time = f32 -> argb.colour

let always: argb.colour -> colour_at_time = const

let cycle (r: f32) (c1: argb.colour) (c2: argb.colour): colour_at_time =
  \t -> let t' = (t - f32.trunc t) * 2f32 * r
        in if t' > 1.0f32 * r
           then argb.mix (r-t'+1f32) c1 (t'-1f32)  c2
           else argb.mix t' c1 (r-t') c2

let circle (c1: colour_at_time) (c2: colour_at_time): image =
  let pixel t (x, y) =
    if f32.sqrt(x**2f32 + y**2f32) < 0.5f32
    then c1 t
    else c2 t
  in {aspect=1f32, pixel}

let above (top: image) (below: image): image =
  let aspect = (f32.max top.aspect below.aspect)/2f32
  let pixel t (x, y) =
    if y < 0f32
    then top.pixel t (x, 0.5f32+y*2f32)
    else below.pixel t (x, y*2f32-0.5f32)
  in {aspect, pixel}

let beside (left: image) (right: image): image =
  let aspect = 2f32 * f32.min left.aspect right.aspect
  let pixel t (x, y) =
    if x < 0f32
    then left.pixel t (0.5f32+x*2f32, y)
    else right.pixel t (x*2f32-0.5f32, y)
  in {aspect, pixel}

let invert ({aspect, pixel=orig_pixel}: image): image =
  let pixel t p = ~(orig_pixel t p)
  in {aspect, pixel}

let speedup (f: f32) ({aspect, pixel=orig_pixel}: image): image =
  let pixel t p = orig_pixel (t*f) p
  in {aspect, pixel}

let rotate_point (d: f32) ((x,y): point) =
  let s = f32.sin d
  let c = f32.cos d
  in (x*c - y*s,
      x*s + y*c)

let spin (r: f32) ({aspect, pixel=orig_pixel}: image): image =
  let pixel t = orig_pixel t <<| rotate_point (r*t)
  in {aspect, pixel}

let test_image =
  let stack = circle (always argb.white) (cycle 1f32 argb.black argb.red) `above`
              circle (cycle 1f32 argb.black argb.red) (always argb.white)
  in spin 1f32 <|
     ((spin (-0.5f32) <|
       (stack `beside` speedup 10f32 (invert stack) `above` (toAnim 1f32 mystique)
        `beside` (toAnim 0.2f32 rbRings)))
      `beside` spin (-2.5f32) (toAnim 0.1f32 (boolToFColor <<| polarChecker 5))
      `above` (spin (0.25f32) ((toAnim 0.3f32 (boolToFColor <<| (coord <||> circ)))
                               `beside` (toAnim 0.3f32 (invY(boolToFColor <<| f))))
               `beside` spin (-0.75f32) (toAnim 0.2f32 (boolToFColor <<| checker))))

entry test_image_render (height: i32) (t: f32) =
  let {aspect, pixel} = test_image
  let width = t32 (aspect * r32 height) -- since aspect == width/height
  let scale (x,y) = (r32 x / r32 width - 0.5f32,
                     r32 y / r32 height - 0.5f32)
  in (width, tabulate_2d width height (curry (scale |>> pixel t)))

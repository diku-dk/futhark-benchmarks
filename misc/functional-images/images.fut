import "/futlib/colour"

type image = { width: i32,
               height: i32,
               pixel: f32 -> (i32, i32) -> argb.colour
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
  let stack = circle 100f32 (always argb.black) (cycle 1f32 argb.black argb.red) `above`
              circle 80f32 (cycle 1f32 argb.black argb.red) (always argb.black)
  in stack `beside` speedup 10f32 (invert stack)

entry test_image_render (t: f32) =
  let {width, height, pixel} = test_image
  in (width, height, tabulate_2d width height (curry (pixel t)))

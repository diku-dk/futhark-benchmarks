import "/futlib/colour"

type image = { width: i32,
               height: i32,
               pixel: (i32, i32) -> argb.colour
             }

let circle (r: f32) (c1: argb.colour) (c2: argb.colour): image =
  let width = t32(r*2f32)
  let height = t32(r*2f32)
  let pixel (x, y) =
    if f32.sqrt((r-r32 x)**2f32 + (r-r32 y)**2f32) < r
    then c1
    else c2
  in {width, height, pixel}

let above (top: image) (below: image): image =
  let width = i32.max top.width below.width
  let height = top.height + below.height
  let pixel (x, y) =
    if x < top.height
    then top.pixel (x,y)
    else below.pixel (x-top.height, y)
  in {width, height, pixel}

let test_image = circle 100f32 argb.white argb.black `above`
                 circle 80f32 argb.black argb.white

entry test_image_render =
  let {width, height, pixel} = test_image
  in (width, height, tabulate_2d width height (flip (curry pixel)))

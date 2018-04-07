-- A wrapper around the Pan library that provides actual screen
-- frames.

import "/futlib/colour"
import "pan"

let fcolorToColour (r,g,b,a) = argb.from_rgba r g b a

let boolToColour = fcolorToColour <<| boolToFColor

entry test_pan_image (screen_width: i32) (screen_height: i32)
                     (width: f32) (height: f32)
                     (xcentre: f32) (ycentre: f32)
                     (t: f32) =
  let aspect_ratio = width / height
  let img = uscale 10f32 <| shiftXor t altRings
  -- Project physical pixel coordinate to position in plane.
  let (xmin,ymin) = ((xcentre - width/2f32),
                     (ycentre - (1f32/aspect_ratio)*width/2f32))
  let (xmax,ymax) = ((xcentre + width/2f32),
                     (ycentre + (1f32/aspect_ratio)*width/2f32))
  let sizex = xmax - xmin
  let sizey = ymax - ymin
  let p (x,y) = (xmin + (r32 x * sizex) / r32 screen_width,
                 ymin + (r32 y * sizey) / r32 screen_height)
  in tabulate_2d screen_width screen_height
     (curry (boolToColour <<| img <<| p))

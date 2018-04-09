-- A wrapper around the Pan library that provides actual screen
-- frames.

import "/futlib/colour"
import "pan"

let fcolorToColour (r,g,b,a) = argb.from_rgba r g b a

let boolToColour = fcolorToColour <<| boolToFColor

-- Fractal definition from "Composing Fractals" by Mark P. Jones.  The
-- main changes are due to the fact that Futhark is not lazy.
let fairlyClose ((u,v): point) = (u*u + v*v) < 100f32

let mandelbrotNext (u,v) (x,y) = (x*x - y*y + u, 2f32*x*y + v)

let mandelbrotEscapes (n: i32): img i32 =
  \p1 -> let continue (p, i) = i < n && fairlyClose p
         let next (p1', i) = (mandelbrotNext p1 p1', i+1)
         in (iterate_while continue next (p1, 0)).2

let juliaEscapes (p0: point) (n: i32): img i32 =
  \p1 -> let continue (p, i) = i < n && fairlyClose p
         let next (p1, i) = (mandelbrotNext p0 p1, i+1)
         in (iterate_while continue next (p1, 0)).2

let mandelbrotSet (n: i32): region =
  (==n) <<| mandelbrotEscapes n

let mandelbrotImage 'color (n: i32) (colourise: i32 -> color): img color =
  colourise <<| mandelbrotEscapes n

let mandelbrotGreyscale (n: i32): cimage =
  mandelbrotImage n <| \i -> let i' = r32 i / r32 n
                             in (i', i', i', 1f32)

let juliaGreyscale (p0: point) (n: i32): cimage =
  juliaEscapes p0 n |>> \i -> let i' = r32 i / r32 n
                              in (i', i', i', 1f32)

entry test_pan_image (screen_width: i32) (screen_height: i32)
                     (width: f32) (height: f32)
                     (xcentre: f32) (ycentre: f32)
                     (xuser: f32) (yuser: f32)
                     (t: f32): [screen_width][screen_height]argb.colour =
  let aspect_ratio = width / height
  let img = fcolorToColour <<| juliaGreyscale (xuser, yuser) 100
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
     (curry (img <<| p))

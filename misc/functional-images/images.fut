-- A wrapper around the Pan library that provides actual screen
-- frames.

default (f32)

import "/futlib/colour"
import "pan"

let fcolorToColour (r,g,b,a) = argb.from_rgba r g b a

let boolToColour: bool -> argb.colour = fcolorToColour <<| boolToFColor

type argb_image = img argb.colour

let cimage_to_argb: cimage -> argb_image = (|>> fcolorToColour)

let region_to_argb: region -> argb_image = (|>> boolToColour)

-- Fractal definition from "Composing Fractals" by Mark P. Jones.  The
-- main changes are due to the fact that Futhark is not lazy.
let fairlyClose ((u,v): point) = (u*u + v*v) < 100f32

import "/futlib/complex"

module complex = complex f32
type complex = complex.complex

let mandelbrotNext p1 p1' = complex.(p1 + p1' * p1')

let mandelbrotEscapes (n: i32): img (complex, i32) =
  \p1 -> let continue (p, i) = i < n && fairlyClose p
         let next (p1', i) = (mandelbrotNext p1 p1', i+1)
         in iterate_while continue next (p1, 0)

let juliaEscapes (p0: point) (n: i32): img (complex, i32) =
  \p1 -> let continue (p, i) = i < n && fairlyClose p
         let next (p1, i) = (mandelbrotNext p0 p1, i+1)
         in iterate_while continue next (p1, 0)

let mandelbrotSet (n: i32): region =
  (\(_, i) -> i==n) <<| mandelbrotEscapes n

let mandelbrotImage 'color (n: i32) (colourise: (complex, i32) -> color): img color =
  colourise <<| mandelbrotEscapes n

let mandelbrotGreyscale (n: i32): cimage =
  mandelbrotImage n <| \(_, i) -> let i' = r32 i / r32 n
                                  in (i', i', i', 1f32)

let visualise_argb_image (img: argb_image)
                         (screen_width: i32) (screen_height: i32)
                         (width: f32) (height: f32)
                         (xcentre: f32) (ycentre: f32) =
  let aspect_ratio = width / height
  -- Project physical pixel coordinate to position in plane.
  let (xmin,ymin) = ((xcentre - width/2f32),
                     (ycentre - (1f32/aspect_ratio)*width/2f32))
  let (xmax,ymax) = ((xcentre + width/2f32),
                     (ycentre + (1f32/aspect_ratio)*width/2f32))
  let sizex = xmax - xmin
  let sizey = ymax - ymin
  let p (x,y) = (xmin + (r32 x * sizex) / r32 screen_width,
                 ymin + (r32 y * sizey) / r32 screen_height)
  in tabulate_2d screen_width screen_height (curry (p |>> img))

let visualise_cimage (img: cimage) = visualise_argb_image (cimage_to_argb img)

let juliaGreyscale (p0: point) (n: i32): cimage =
  juliaEscapes p0 n |>> \(_, i) -> let i' = r32 i / r32 n
                                   in (i', i', i', 1f32)

-- | Cubic interpolation.
let cubic (x0:f32,x1:f32) (y0:f32,y1:f32) (m0:f32,m1:f32) (x:f32) =
  let h    = x1 - x0
  let t    = (x - x0) / h
  let h_00 = (1.0 + 2.0*t) * (1.0 - t) ** 2.0
  let h_10 = t * (1.0 - t) ** 2.0
  let h_01 = t ** 2.0 * (3.0 - 2.0 * t)
  let h_11 = t ** 2.0 * (t - 1.0)
  in y0 * h_00 + h * m0 * h_10 + y1 * h_01 + h * m1 * h_11

let interp (x0:f32, x1:f32)
           (y0:argb.colour, y1:argb.colour)
           ((mr0,mg0,mb0):(f32,f32,f32),
            (mr1,mg1,mb1):(f32,f32,f32))
           (x: f32) =
  let (r0,g0,b0,_) = argb.to_rgba y0
  let (r1,g1,b1,_) = argb.to_rgba y1
  in argb.from_rgba (cubic (x0,x1) (r0,r1) (mr0,mr1) x)
                    (cubic (x0,x1) (g0,g1) (mg0,mg1) x)
                    (cubic (x0,x1) (b0,b1) (mb0,mb1) x)
                    0.0

-- the ultraPalette from Accelerate.
let mk_palette (points: i32) (ix: i32): argb.colour =
  let p = r32 ix / r32 points

  let p0 = 0.0
  let p1 = 0.16
  let p2 = 0.42
  let p3 = 0.6425
  let p4 = 0.8575
  let p5 = 1.0

  let rgb8 (r: i32) (g: i32) (b: i32) =
    argb.from_rgba (r32 r / 255.0) (r32 g / 255.0) (r32 b / 255.0) 0.0

  let c0 = rgb8 0   7   100
  let c1 = rgb8 32  107 203
  let c2 = rgb8 237 255 255
  let c3 = rgb8 255 170 0
  let c4 = rgb8 0   2   0
  let c5 = c0

  let m0 = (0.7843138, 2.4509804,  2.52451)
  let m1 = (1.93816,   2.341629,   1.6544118)
  let m2 = (1.7046283, 0.0,        0.0)
  let m3 = (0.0,       -2.2812111, 0.0)
  let m4 = (0.0,       0.0,        0.0)
  let m5 = m0

  in
  if p <= p1 then interp (p0,p1) (c0,c1) (m0,m1) p else
  if p <= p2 then interp (p1,p2) (c1,c2) (m1,m2) p else
  if p <= p3 then interp (p2,p3) (c2,c3) (m2,m3) p else
  if p <= p4 then interp (p3,p4) (c3,c4) (m3,m4) p else
                  interp (p4,p5) (c4,c5) (m4,m5) p

let log2 (x: f32) = f32.log x / f32.log 2.0

let escape_to_colour (limit: i32) (points: i32)
                     (z: complex, n: i32): argb.colour =
  if limit == n then argb.black
  else let smooth = log2 (log2 (complex.mag z))
       let scale = 256.0
       let shift = 1664.0
       let ix = t32 (f32.sqrt (r32 n + 1.0 - smooth) * scale + shift)
       in mk_palette points (ix %% points)

entry mandelbrot_greyscale (_: f32) (_: f32) t =
  mandelbrotGreyscale 100 |> rotate' t |> visualise_cimage

entry julia_greyscale (xuser: f32) (yuser: f32) t =
  juliaGreyscale (xuser, yuser) 100 |> rotate' t |> visualise_cimage

entry mandelbrot_colour (_: f32) (_: f32) t =
  mandelbrotImage 100 (escape_to_colour 100 2048) |>
  rotate' t |> visualise_argb_image

entry julia_colour (user_x: f32) (user_y: f32) t =
  juliaEscapes (user_x, user_y) 100
  |> rotate' t |>> escape_to_colour 100 2048
  |> visualise_argb_image

entry figure_7_15 (_: f32) (_: f32) t =
  altRings |> shiftXor t |> uscale 0.1f32
  |>> boolToColour
  |> visualise_argb_image

entry fancy (_: f32) (_: f32) t =
  cond altRings ((altRings |> shiftXor t |> uscale 0.1f32 |> region_to_argb) : argb_image)
                ((mandelbrotGreyscale 100 |> rotate' t |> cimage_to_argb): argb_image)
  |> visualise_argb_image

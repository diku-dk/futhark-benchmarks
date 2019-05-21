-- Buddhabrot fractal: https://en.wikipedia.org/wiki/Buddhabrot

import "lib/github.com/athas/matte/colour"
import "lib/github.com/diku-dk/complex/complex"
import "lib/github.com/diku-dk/cpprandom/random"

module rand = minstd_rand
module dist = uniform_real_distribution f32 rand
type rng = rand.rng

module c32 = mk_complex f32
type c32 = c32.complex

let divergence (radius: f32) (limit: i32) (c0: c32): ([limit]c32, bool) =
  let trajectory = replicate limit (0.0, 0.0)
  let (trajectory, _, i) =
    loop (trajectory, c, i) = (trajectory, c0, 0)
    while i < limit && c32.mag c < radius do
      unsafe
      let c' = c32.(c0+ c * c)
      let trajectory[i] = c'
      in (trajectory, c', i + 1)
  in (trajectory, i == limit)

let trajectories (rng: rng) (limit: i32) (radius: f32)
                 (npoints: i32) (field: (f32,f32,f32,f32)): (rng, []([limit]c32, bool)) =
  let (xmin, ymin, xmax, ymax) = field
  let mk_point rng = let (rng, x) = dist.rand (xmin, xmax) rng
                     let (rng, y) = dist.rand (ymin, ymax) rng
                     in (rng, c32.mk x y)
  let (rngs, points) = rng |> rand.split_rng npoints |> map mk_point |> unzip
  let sample_point c0 = divergence radius limit c0
  in (rand.join_rng rngs, map sample_point points)

-- This colouring function makes no sense, but it creates a cool retro effect.
let colourise(max_visits: i32) (visits: i32): i32 =
  i32.u32 (f32.to_bits ((r32 visits) / (r32 max_visits) * 255.0))

let pixel_pos screenX screenY field c =
  let (xmin, ymin, xmax, ymax) = field
  let sizex = xmax - xmin
  let sizey = ymax - ymin
  let (y,x) = (c32.re c, c32.im c)
  in (t32 ((x - xmin) * (r32 screenX / sizex)),
      t32 ((y - ymin) * (r32 screenY / sizey)))

let row_major screenX _screenY (x, y): i32 =
  y * screenX + x

type state = { rng: rand.rng
             , prior_visits: []i32
             , iter: i32 }

entry new_state (screenX: i32) (screenY: i32) (seed: i32): state =
  { rng = rand.rng_from_seed [seed]
  , prior_visits = replicate (screenX*screenY) 0
  , iter = 0 }

let visualise (state: state)
              (screenX: i32) (screenY: i32) (limit: i32) (radius: f32)
              (npoints: i32) (field: (f32,f32,f32,f32)):
             (state, [screenY][screenX]argb.colour) =
  let (rng, trajectories) = trajectories state.rng limit radius npoints field
  let mk_increments (npoints, escaped) =
    if escaped
    then map (pixel_pos screenX screenY field >-> row_major screenX screenY) npoints
    else replicate limit (-1)
  let touched = flatten (map mk_increments trajectories)
  let visits_per_pixel = reduce_by_index (copy state.prior_visits) (+) 0
                                         touched (map (const 1) touched)
  let max_visits = i32.maximum visits_per_pixel
  let coloured = map (colourise max_visits) visits_per_pixel
  in ({rng, prior_visits = visits_per_pixel, iter = state.iter + 1},
      unflatten screenY screenX coloured)

entry main (state: state) (screenX: i32) (screenY: i32)
         xcentre ycentre width limit radius npoints
       : (state, [screenY][screenX]argb.colour) =
  let aspect_ratio = r32 screenX / r32 screenY
  let (xmin,ymin) = (xcentre - width/2,
                     ycentre - (1/aspect_ratio)*width/2)
  let (xmax,ymax) = (xcentre + width/2,
                     ycentre + (1/aspect_ratio)*width/2)
  in visualise state screenX screenY limit radius npoints (xmin, ymin, xmax, ymax)

entry frob (s: state) = s.iter

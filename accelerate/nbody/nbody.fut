-- N-body simulation based on the one from Accelerate:
-- https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/n-body
--
-- ==
-- tags { futhark-opencl futhark-c }
--
-- nobench input @ data/nbody-acc-t0.in
-- output @ data/nbody-acc-t0.out
--
-- nobench input @ data/nbody-acc-t10.in
-- output @ data/nbody-acc-t10.out
--
-- nobench input @ data/100-bodies.in
-- input @ data/1000-bodies.in
-- input @ data/10000-bodies.in
-- input @ data/100000-bodies.in

-- "data/N-bodies.in" all have the other attributes n_steps=1, timestep=1.0, and
-- epsilon=50.0.

import "lib/github.com/athas/vector/vspace"
import "lib/github.com/athas/matte/colour"

module vec3 = mk_vspace_3d f32

type mass = f32
type position = vec3.vector
type acceleration = vec3.vector
type velocity = vec3.vector
type body = {position: position,
             mass: mass,
             velocity: velocity,
             acceleration: acceleration}

let accel (epsilon: f32) (x:body) (y: body): velocity =
  let r = vec3.(y.position - x.position)
  let rsqr = vec3.dot r r + epsilon * epsilon
  let invr = 1.0f32 / f32.sqrt rsqr
  let invr3 = invr * invr * invr
  let s = y.mass * invr3
  in vec3.scale s r

let calc_accels (epsilon: f32) (bodies: []body): []acceleration =
  let move (body: body) =
    let accels = map (accel epsilon body) bodies
    in reduce_comm (vec3.+) {x=0f32, y=0f32, z=0f32} accels
  in map move bodies

let advance_body (time_step: f32) (body: body) (acc: acceleration): body =
  let acceleration = vec3.scale body.mass acc
  let position = vec3.(body.position + scale time_step body.velocity)
  let velocity = vec3.(body.velocity + scale time_step acceleration)
  in {position, mass=body.mass, velocity, acceleration}

let advance_bodies [n] (epsilon: f32) (time_step: f32) (bodies: [n]body): [n]body =
  let accels = calc_accels epsilon bodies
  in map2 (advance_body time_step) bodies accels

let advance_bodies_steps [n] (n_steps: i32) (epsilon: f32) (time_step: f32)
                             (bodies: [n]body): [n]body =
  loop bodies for _i < n_steps do
    advance_bodies epsilon time_step bodies

let wrap_body (posx: f32, posy: f32, posz: f32)
              (mass: f32)
              (velx: f32, vely: f32, velz: f32)
              (accx: f32, accy: f32, accz: f32): body =
  {position={x=posx, y=posy, z=posz},
   mass,
   velocity={x=velx, y=vely, z=velz},
   acceleration={x=accx, y=accy, z=accz}}

let unwrap_body (b: body): ((f32, f32, f32), f32, (f32, f32, f32), (f32, f32, f32)) =
  ((b.position.x, b.position.y, b.position.z),
   b.mass,
   (b.velocity.x, b.velocity.y, b.velocity.z),
   (b.acceleration.x, b.acceleration.y, b.acceleration.z))

let main [n]
        (n_steps: i32,
         epsilon: f32,
         time_step: f32,
         xps: [n]f32,
         yps: [n]f32,
         zps: [n]f32,
         ms: [n]f32,
         xvs: [n]f32,
         yvs: [n]f32,
         zvs: [n]f32,
         xas: [n]f32,
         yas: [n]f32,
         zas: [n]f32): ([n]f32, [n]f32, [n]f32, [n]f32, [n]f32, [n]f32, [n]f32, [n]f32, [n]f32, [n]f32) =
  let bodies  = map4 wrap_body (zip3 xps yps zps) ms (zip3 xvs yvs zvs) (zip3 xas yas zas)
  let bodies' = advance_bodies_steps n_steps epsilon time_step bodies
  let (final_pos, ms', final_vel, final_acc) = map unwrap_body (bodies') |> unzip4
  let (xps', yps', zps') = unzip3 final_pos
  let (xvs', yvs', zvs') = unzip3 final_vel
  let (xas', yas', zas') = unzip3 final_acc
  in (xps', yps', zps', ms', xvs', yvs', zvs', xas', yas', zas')

let rotatePointByMatrix (rotation: [3][3]f32) ({x,y,z}: position): position =
  {x= x*rotation[0,0] + y*rotation[1,0] + z*rotation[2,0],
   y= x*rotation[0,1] + y*rotation[1,1] + z*rotation[2,1],
   z= x*rotation[0,2] + y*rotation[1,2] + z*rotation[2,2]}

let rotatePointsByMatrix [n] (rotation: [3][3]f32)(ps: [n]position): [n]position =
  map (rotatePointByMatrix rotation) ps

let rotateXMatrix (angle: f32): [3][3]f32 =
  [[1f32,        0f32,         0f32],
   [0f32, f32.cos angle, -f32.sin angle],
   [0f32, f32.sin angle,  f32.cos angle]]

let rotateYMatrix (angle: f32): [3][3]f32 =
  [[f32.cos angle,  0f32, f32.sin angle],
   [0f32,           1f32, 0f32],
   [-f32.sin angle, 0f32, f32.cos angle]]

let matmult [n][m][p] (x: [n][m]f32) (y: [m][p]f32): [n][p]f32 =
  map (\xr ->
        map (\yc -> f32.sum (map2 (*) xr yc))
            (transpose y))
      x

let rotationMatrix (x_rotation: f32) (y_rotation: f32): [3][3]f32 =
  matmult (rotateXMatrix x_rotation) (rotateYMatrix y_rotation)

let inverseRotationMatrix (x_rotation: f32) (y_rotation: f32): [3][3]f32 =
  matmult (rotateYMatrix y_rotation) (rotateXMatrix x_rotation)

entry inverseRotatePoint (x: f32, y: f32, z: f32, x_rotation: f32, y_rotation: f32): (f32,f32,f32) =
  let {x,z,y} = rotatePointByMatrix (inverseRotationMatrix x_rotation y_rotation) {x,y,z}
  in (x,y,z)

let rotatePoints [n] (ps: [n]position) (x_rotation: f32) (y_rotation: f32): [n]position =
  rotatePointsByMatrix (rotationMatrix x_rotation y_rotation) ps

let renderPoint(w: i32, h: i32, x_ul: f32, y_ul: f32, x_br: f32, y_br: f32, max_mass: f32)
               ({x,y,z=_}:position) (m: f32): (i32, i32) =
  -- Draw nothing if the point is outside the viewport.
  if x < x_ul || x > x_br || y < y_ul || y > y_br then (-1, 0)
  else
    -- Normalise x,y to positions in interval (0,1) within the viewport.
    let x' = (x-x_ul) / (x_br-x_ul)
    let y' = (y-y_ul) / (y_br-y_ul)
    -- Convert x',y' to screen coordinate space.
    let x'' = t32(x' * r32(w))
    let y'' = t32(y' * r32(h))
    let intensity = if m >= max_mass
                    then 255
                    else 128 + t32((m / max_mass) * 128f32)
    let colour = intensity * 0x10000 +
                 intensity * 0x100 +
                 0xFF
    in (x''*h + y'', colour)

entry render [n]
            (w: i32, h: i32, x_ul: f32, y_ul: f32, x_br: f32, y_br: f32,
             xps: [n]f32, yps: [n]f32, zps: [n]f32, ms: [n]f32,
             x_rotation: f32, y_rotation: f32,
             max_mass: f32, invert: bool): [w][h]i32 =
  let background = if invert then argb.white else argb.black
  let (is, vs) = unzip(map2 (renderPoint(w,h,x_ul,y_ul,x_br,y_br,max_mass))
                       (rotatePoints (map3 (\x y z -> {x,y,z}) xps yps zps)
                                     x_rotation y_rotation) ms)
  let vs' = map (\x -> if invert then ~x else x) vs
  in unflatten w h (scatter (replicate (w*h) background) is vs')

entry mouse_mass_active (xps: *[]f32) (yps: *[]f32) (zps: *[]f32) (ms: *[]f32) (x: f32) (y: f32) (z: f32) =
  (xps with [0] <- x,
   yps with [0] <- y,
   zps with [0] <- z,
   ms with [0] <- 10000)

entry mouse_mass_inactive (xps: *[]f32) (yps: *[]f32) (zps: *[]f32) (ms: *[]f32) =
  (xps with [0] <- 0,
   yps with [0] <- 0,
   zps with [0] <- 0,
   ms with [0] <- 0.0001)

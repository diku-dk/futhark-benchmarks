-- Barnest-Hut N-body simulation.
--
-- This is the work of Sebastian Posselt, Thomas Veje Christensen, and
-- Malte Sølvsten Velin.
--
-- The benchmarking is hardcoded to use theta=0.5.
-- ==
-- notest input @ data/1000-bodies.in
-- notest input @ data/10000-bodies.in
-- notest input @ data/100000-bodies.in

import "octree"
import "physics"

let move [n] (epsilon: f32) (theta: f32)
             (t: [n]octnode, side_max: f32, root_delta: i32) (x: pointmass)
             : acceleration =
  let (acceleration, _, _, _) =
  loop (acc, cur, prev, to_check) = ({x=0f32, y=0f32, z=0f32}, 0, -1, 1)
  while to_check > 0 do
    let from_child = prev != -1 && unsafe t[prev].parent == cur
    let current = unsafe t[cur]
    in
    if from_child then -- prev is my child => just move to next node
      --- index 'i' of child will be found before 'i' goes out of bounds.
      let j = loop i = 0 while (unsafe (current.children)[i]) != prev do
                         (i + 1)
      let next_cur =
        if j == 7 || (unsafe (current.children)[j + 1]) == -1
          then current.parent
        else unsafe (current.children)[j + 1] --- j must be less than 7
      in (acc, next_cur, cur, to_check)
    else -- prev is my parent
      --let s = side_max / 2.0f32**(f32.i32 current.tree_level)
      let s = side_max / f32.i32 (2i32**current.tree_level)
      let position = current.body.position
      let total_mass = current.body.mass
      let q = vec3.scale (1.0f32 / total_mass) position
      let p = x.position
      let inner =
        let x = (p.x - q.x)
        let y = (p.y - q.y)
        let z = (p.z - q.z)
        in x*x + y*y + z*z
      let local_theta =  s / f32.sqrt inner
      let overlapping_subtree = (root_delta / 3) + current.tree_level > 11
      in
      -- node is far enough, so we update
      if current.is_leaf || overlapping_subtree || local_theta < theta then
        let velocity = accel epsilon x {position = q, mass = total_mass}
        in (acc vec3.+ velocity, current.parent, cur, to_check - 1)
      else -- node is too close
        let children = current.children
        let num_children = map (i32.bool <-< (>= 0)) children
                           |> reduce_comm (+) 0
        in (acc, children[0], cur, to_check + num_children - 1)
  in acceleration

let calc_accels [n] (epsilon: f32) (theta: f32) (bodies: [n]body)
                    : ([n]acceleration, [n]body) =
  let (accelerator, side_max, root_delta, sorted_bodies) =
    mk_accelerator bodies
  in (map (move epsilon theta (accelerator, side_max, root_delta))
          (map pointmass sorted_bodies),
      sorted_bodies)

let advance_bodies [n] (theta: f32) (epsilon: f32) (time_step: f32)
                       (bodies: [n]body): [n]body =
  let (accels, bodies) = calc_accels epsilon theta bodies
  in map2 (advance_body time_step) bodies accels

let advance_bodies_steps [n] (n_steps: i32) (theta: f32) (epsilon: f32) (time_step: f32)
                             (bodies: [n]body): [n]body =
  loop bodies for _i < n_steps do
    advance_bodies theta epsilon time_step bodies

let wrap_body (posx: f32, posy: f32, posz: f32)
              (mass: f32)
              (velx: f32, vely: f32, velz: f32): body =
  {position={x=posx, y=posy, z=posz},
   mass,
   velocity={x=velx, y=vely, z=velz}}

let unwrap_body (b: body): ((f32, f32, f32), f32, (f32, f32, f32)) =
  ((b.position.x, b.position.y, b.position.z),
   b.mass,
   (b.velocity.x, b.velocity.y, b.velocity.z))

entry main [n]
        (n_steps: i32)
        (epsilon: f32)
        (time_step: f32)
        (xps: [n]f32)
        (yps: [n]f32)
        (zps: [n]f32)
        (ms: [n]f32)
        (xvs: [n]f32)
        (yvs: [n]f32)
        (zvs: [n]f32): ([n]f32, [n]f32, [n]f32, [n]f32, [n]f32, [n]f32, [n]f32) =
  let theta = 0.5
  let bodies  = map3 wrap_body (zip3 xps yps zps) ms (zip3 xvs yvs zvs)
  let bodies' = advance_bodies_steps n_steps theta epsilon time_step bodies
  let (final_pos, ms', final_vel) = map unwrap_body (bodies') |> unzip3
  let (xps', yps', zps') = unzip3 final_pos
  let (xvs', yvs', zvs') = unzip3 final_vel
  in (xps', yps', zps', ms', xvs', yvs', zvs')

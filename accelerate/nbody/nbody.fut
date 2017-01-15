-- N-body simulation based on the one from Accelerate:
-- https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/n-body
--
-- ==
-- tags { futhark-opencl futhark-c }
--
-- input @ nbody-acc-t0.in
-- output @ nbody-acc-t0.out
--
-- input @ nbody-acc-t10.in
-- output @ nbody-acc-t10.out
--
-- notravis input @ nbody-n_steps=1-n_bodies=100-timestep=1.0-epsilon=50.0.in
-- notravis input @ nbody-n_steps=1-n_bodies=1000-timestep=1.0-epsilon=50.0.in
-- notravis input @ nbody-n_steps=1-n_bodies=10000-timestep=1.0-epsilon=50.0.in
-- notravis input @ nbody-n_steps=1-n_bodies=100000-timestep=1.0-epsilon=50.0.in

type mass = f32
type vec3 = (f32, f32, f32)
type position = vec3
type acceleration = vec3
type velocity = vec3
type body = (position, mass, velocity, acceleration)

fun vec_add((x1, y1, z1): vec3) ((x2, y2, z2): vec3): vec3 =
  (x1 + x2, y1 + y2, z1 + z2)

fun vec_subtract((x1, y1, z1): vec3, (x2, y2, z2): vec3): vec3 =
  (x1 - x2, y1 - y2, z1 - z2)

fun vec_mult_factor(factor: f32, (x, y, z): vec3): vec3 =
  (x * factor, y * factor, z * factor)

fun dot((x1, y1, z1): vec3, (x2, y2, z2): vec3): f32 =
  x1 * x2 + y1 * y2 + z1 * z2

fun accel (epsilon: f32) ((pi, _, _ , _):body) ((pj, mj, _ , _): body)
          : velocity =
  let r = vec_subtract(pj, pi)
  let rsqr = dot(r, r) + epsilon * epsilon
  let invr = 1.0f32 / sqrt32(rsqr)
  let invr3 = invr * invr * invr
  let s = mj * invr3
  in vec_mult_factor(s, r)

fun move(epsilon: f32, bodies: []body) (this_body: body): position =
  let accels = map (accel epsilon this_body) bodies
  in reduceComm vec_add (0f32, 0f32, 0f32) accels

fun calc_accels(epsilon: f32, bodies: []body): []acceleration =
  map (move(epsilon, bodies)) bodies

fun advance_body(time_step: f32) ((pos, mass, vel, _):body) (acc:acceleration): body =
  let acc' = vec_mult_factor(mass, acc)
  let pos' = vec_add pos(vec_mult_factor(time_step, vel))
  let vel' = vec_add vel(vec_mult_factor(time_step, acc'))
  in (pos', mass, vel', acc')

fun advance_bodies(epsilon: f32, time_step: f32, bodies: [n]body): [n]body =
  let accels = calc_accels(epsilon, bodies)
  in map (advance_body time_step) bodies accels

fun advance_bodies_steps(n_steps: i32, epsilon: f32, time_step: f32,
                         bodies: [n]body): [n]body =
  loop (bodies) = for _i < n_steps do
    advance_bodies(epsilon, time_step, bodies)
  in bodies

fun wrap_body (posx: f32, posy: f32, posz: f32,
               mass: f32,
               velx: f32, vely: f32, velz: f32,
               accx: f32, accy: f32, accz: f32): body =
  ((posx, posy, posz), mass, (velx, vely, velz), (accx, accy, accz))

fun unwrap_body(((posx, posy, posz), mass, (velx, vely, velz), (accx, accy, accz)): body)
  : (f32, f32, f32, f32, f32, f32, f32, f32, f32, f32) =
  (posx, posy, posz, mass, velx, vely, velz, accx, accy, accz)

fun main(n_steps: i32,
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
  let bodies  = map wrap_body (zip xps yps zps ms xvs yvs zvs xas yas zas)
  let bodies' = advance_bodies_steps(n_steps, epsilon, time_step, bodies)
  let bodies'' = map unwrap_body (bodies')
  in unzip(bodies'')

fun rotatePointByMatrix (rotation: [3][3]f32) ((x,y,z): position): position =
  (x*rotation[0,0] + y*rotation[1,0] + z*rotation[2,0],
   x*rotation[0,1] + y*rotation[1,1] + z*rotation[2,1],
   x*rotation[0,2] + y*rotation[1,2] + z*rotation[2,2])

fun rotatePointsByMatrix (rotation: [3][3]f32)(ps: [n]position): [n]position =
  map (rotatePointByMatrix rotation) ps

fun rotateXMatrix (angle: f32): [3][3]f32 =
  [[1f32,        0f32,         0f32],
   [0f32, cos32 angle, -sin32 angle],
   [0f32, sin32 angle,  cos32 angle]]

fun rotateYMatrix (angle: f32): [3][3]f32 =
  [[cos32 angle,  0f32, sin32 angle],
   [0f32,         1f32,        0f32],
   [-sin32 angle, 0f32, cos32 angle]]

fun rotationMatrix (x_rotation: f32) (y_rotation: f32): [3][3]f32 =
  matmult (rotateXMatrix x_rotation) (rotateYMatrix y_rotation)

fun inverseRotationMatrix (x_rotation: f32) (y_rotation: f32): [3][3]f32 =
  matmult (rotateYMatrix y_rotation) (rotateXMatrix x_rotation)

entry inverseRotatePoint (x: f32, y: f32, z: f32, x_rotation: f32, y_rotation: f32): position =
  rotatePointByMatrix (inverseRotationMatrix x_rotation y_rotation) (x,y,z)

fun rotatePoints(ps: [n]position) (x_rotation: f32) (y_rotation: f32): [n]position =
  rotatePointsByMatrix (rotationMatrix x_rotation y_rotation) ps

entry render(w: i32, h: i32, x_ul: f32, y_ul: f32, x_br: f32, y_br: f32,
             xps: [n]f32, yps: [n]f32, zps: [n]f32, ms: [n]f32,
             x_rotation: f32, y_rotation: f32,
             max_mass: f32): [w][h]i32 =
  let (is, vs) = unzip(map (renderPoint(w,h,x_ul,y_ul,x_br,y_br,max_mass))
                       (rotatePoints (zip xps yps zps) x_rotation y_rotation) ms)
  in reshape (w,h) (write is vs (replicate (w*h) 0))

fun renderPoint(w: i32, h: i32, x_ul: f32, y_ul: f32, x_br: f32, y_br: f32, max_mass: f32)
               ((x,y,_z):position) (m: f32): (i32, i32) =
  -- Draw nothing if the point is outside the viewport.
  if x < x_ul || x > x_br || y < y_ul || y > y_br then (-1, 0)
  else
    -- Normalise x,y to positions in interval (0,1) within the viewport.
    let x' = (x-x_ul) / (x_br-x_ul)
    let y' = (y-y_ul) / (y_br-y_ul)
    -- Convert x',y' to screen coordinate space.
    let x'' = i32(x' * f32(w))
    let y'' = i32(y' * f32(h))
    let intensity = if m >= max_mass
                    then 255
                    else 128 + i32((m / max_mass) * 128f32)
    let colour = intensity * 0x10000 +
                 intensity * 0x100 +
                 0xFF
    in (x''*h + y'', colour)

fun matmult(x: [n][m]f32) (y: [m][p]f32): [n][p]f32 =
  map (\(xr) ->
        map (\(yc) -> reduce (+) 0f32 (map (*) xr yc))
            (transpose(y)))
      x

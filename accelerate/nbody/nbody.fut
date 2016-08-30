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

fun accel (epsilon: f32) ((pi, mi, _ , _):body) ((pj, mj, _ , _): body)
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
  in zipWith (advance_body time_step) bodies accels

fun advance_bodies_steps(n_steps: i32, epsilon: f32, time_step: f32,
                         bodies: [n]body): [n]body =
  loop (bodies) = for i < n_steps do
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

fun rotateByMatrix(ps: [n]position) (rotation: [3][3]f32): [n]position =
  map (fn (x,y,z) =>
         (x*rotation[0,0] + x*rotation[1,0] + y*rotation[2,0],
          y*rotation[0,1] + y*rotation[1,1] + y*rotation[2,1],
          z*rotation[0,2] + z*rotation[1,2] + z*rotation[2,2]))
  ps

fun rotateX(ps: [n]position) (angle: f32): [n]position =
  rotateByMatrix ps (transpose([[1f32, 0f32, 0f32],
                                [0f32, cos32 angle, -sin32 angle],
                                [0f32, sin32 angle, cos32 angle]]))

fun rotateY(ps: [n]position) (angle: f32): [n]position =
  rotateByMatrix ps (transpose([[cos32 angle, 0f32, sin32 angle],
                                [0f32, 1f32, 0f32],
                                [-sin32 angle, 0f32, cos32 angle]]))


entry render(w: int, h: int, x_ul: f32, y_ul: f32, x_br: f32, y_br: f32,
             xps: [n]f32, yps: [n]f32, zps: [n]f32,
             x_rotation: f32, y_rotation: f32): [w][h]int =
  let (is, vs) = unzip(map (renderPoint(w,h,x_ul,y_ul,x_br,y_br))
                       (rotateY (rotateX (zip xps yps zps) x_rotation) y_rotation))
  in reshape (w,h) (write is vs (replicate (w*h) 0))

fun renderPoint(w: int, h: int, x_ul: f32, y_ul: f32, x_br: f32, y_br: f32)
  ((x,y,z):position): (int, int) =
  -- Draw nothing if the point is outside the viewport.
  if x < x_ul || x > x_br || y < y_ul || y > y_br then (-1, 0)
  else
    -- Normalise x,y to positions in interval (0,1) within the viewport.
    let x' = (x-x_ul) / (x_br-x_ul)
    let y' = (y-y_ul) / (y_br-y_ul)
    -- Convert x',y' to screen coordinate space.
    let x'' = int(x' * f32(w))
    let y'' = int(y' * f32(h))
    in (x''*h + y'', 0x00FFFFFF)

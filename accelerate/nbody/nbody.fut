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
type vec3 = {f32, f32, f32}
type position = vec3
type acceleration = vec3
type velocity = vec3
type body = {position, mass, velocity, acceleration}

fun vec3 vec_add(vec3 v1, vec3 v2) =
  let {x1, y1, z1} = v1
  let {x2, y2, z2} = v2
  in {x1 + x2, y1 + y2, z1 + z2}

fun vec3 vec_subtract(vec3 v1, vec3 v2) =
  let {x1, y1, z1} = v1
  let {x2, y2, z2} = v2
  in {x1 - x2, y1 - y2, z1 - z2}

fun vec3 vec_mult_factor(f32 factor, vec3 v) =
  let {x, y, z} = v
  in {x * factor, y * factor, z * factor}

fun f32 dot(vec3 v1, vec3 v2) =
  let {x1, y1, z1} = v1
  let {x2, y2, z2} = v2
  in x1 * x2 + y1 * y2 + z1 * z2

fun velocity accel(f32 epsilon, vec3 pi, f32 mi, vec3 pj, f32 mj) =
  let r = vec_subtract(pj, pi)
  let rsqr = dot(r, r) + epsilon * epsilon
  let invr = 1.0f32 / sqrt32(rsqr)
  let invr3 = invr * invr * invr
  let s = mj * invr3
  in vec_mult_factor(s, r)

fun vec3 accel_wrap(f32 epsilon, body body_i, body body_j) =
  let {pi, mi, _ , _} = body_i
  let {pj, mj, _ , _} = body_j
  in accel(epsilon, pi, mi, pj, mj)

fun position move(f32 epsilon, [body] bodies, body this_body) =
  let accels = map(fn acceleration (body other_body) =>
                     accel_wrap(epsilon, this_body, other_body),
                   bodies)
  in reduceComm(vec_add, {0f32, 0f32, 0f32}, accels)

fun [acceleration] calc_accels(f32 epsilon, [body] bodies) =
  map(move(epsilon, bodies), bodies)

fun body advance_body(f32 time_step, body this_body) =
  let {pos, mass, vel, acc} = this_body
  let pos' = vec_add(pos, vec_mult_factor(time_step, vel))
  let vel' = vec_add(vel, vec_mult_factor(time_step, acc))
  let {xp', yp', zp'} = pos'
  let {xv', yv', zv'} = vel'
  in {pos', mass, vel', acc}

fun body advance_body_wrap(f32 time_step, body this_body, acceleration accel) =
  let {pos, mass, vel, acc} = this_body
  let accel' = vec_mult_factor(mass, accel)
  let body' = {pos, mass, vel, accel'}
  in advance_body(time_step, body')

fun [body, n] advance_bodies(f32 epsilon, f32 time_step, [body, n] bodies) =
  let accels = calc_accels(epsilon, bodies)
  in zipWith(advance_body_wrap(time_step), bodies, accels)

fun [body, n] advance_bodies_steps(i32 n_steps, f32 epsilon, f32 time_step,
                                   [body, n] bodies) =
  loop (bodies) = for i < n_steps do
    advance_bodies(epsilon, time_step, bodies)
  in bodies

fun body wrap_body (f32 posx, f32 posy, f32 posz,
                    f32 mass,
                    f32 velx, f32 vely, f32 velz,
                    f32 accx, f32 accy, f32 accz) =
  {{posx, posy, posz}, mass, {velx, vely, velz}, {accx, accy, accz}}

fun {f32, f32, f32, f32, f32, f32, f32, f32, f32, f32} unwrap_body(body this_body) =
  let {{posx, posy, posz}, mass, {velx, vely, velz}, {accx, accy, accz}} = this_body
  in {posx, posy, posz, mass, velx, vely, velz, accx, accy, accz}



fun {[f32, n], [f32, n], [f32, n], [f32, n], [f32, n], [f32, n], [f32, n], [f32, n], [f32, n], [f32, n]}
  main(i32 n_steps,
       f32 epsilon,
       f32 time_step,
       [f32, n] xps,
       [f32, n] yps,
       [f32, n] zps,
       [f32, n] ms,
       [f32, n] xvs,
       [f32, n] yvs,
       [f32, n] zvs,
       [f32, n] xas,
       [f32, n] yas,
       [f32, n] zas) =
  let bodies  = map(wrap_body, zip(xps, yps, zps, ms, xvs, yvs, zvs, xas, yas, zas))
  let bodies' = advance_bodies_steps(n_steps, epsilon, time_step, bodies)
  let bodies'' = map(unwrap_body, bodies')
   in unzip(bodies'')

-- N-body simulation based on the one from Accelerate:
-- https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/n-body
--
-- Type descriptions:
--
-- type mass = f32
-- type position = {f32, f32, f32}
-- type acceleration = {f32, f32, f32}
-- type velocity = {f32, f32, f32}
-- type body = (position, mass, velocity, acceleration)
--           =~ {f32, f32, f32, -- position
--               f32,           -- mass
--               f32, f32, f32, -- velocity
--               f32, f32, f32} -- acceleration
--
-- ==
--
-- input @ nbody-acc-t0.in
-- output @ nbody-acc-t0.out
--
-- input @ nbody-acc-t10.in
-- output @ nbody-acc-t10.out
--
-- compiled input @ nbody-n_steps=1-n_bodies=100-timestep=1.0-epsilon=50.0.in
-- compiled input @ nbody-n_steps=1-n_bodies=1000-timestep=1.0-epsilon=50.0.in
-- compiled input @ nbody-n_steps=1-n_bodies=10000-timestep=1.0-epsilon=50.0.in
-- compiled input @ nbody-n_steps=1-n_bodies=100000-timestep=1.0-epsilon=50.0.in

fun {f32, f32, f32}
  vec_add({f32, f32, f32} v1,
               {f32, f32, f32} v2) =
  let {x1, y1, z1} = v1
  let {x2, y2, z2} = v2
  in {x1 + x2, y1 + y2, z1 + z2}

fun {f32, f32, f32}
  vec_subtract({f32, f32, f32} v1,
               {f32, f32, f32} v2) =
  let {x1, y1, z1} = v1
  let {x2, y2, z2} = v2
  in {x1 - x2, y1 - y2, z1 - z2}

fun {f32, f32, f32}
  vec_mult_factor(f32 factor,
                  {f32, f32, f32} v) =
  let {x, y, z} = v
  in {x * factor, y * factor, z * factor}

fun f32
  dot({f32, f32, f32} v1,
      {f32, f32, f32} v2) =
  let {x1, y1, z1} = v1
  let {x2, y2, z2} = v2
  in x1 * x2 + y1 * y2 + z1 * z2
  
fun {f32, f32, f32}
  accel(f32 epsilon,
        {f32, f32, f32} pi,
        f32 mi,
        {f32, f32, f32} pj,
        f32 mj) =
  let r = vec_subtract(pj, pi)
  let rsqr = dot(r, r) + epsilon * epsilon
  let invr = 1.0f32 / sqrt32(rsqr)
  let invr3 = invr * invr * invr
  let s = mj * invr3
  in vec_mult_factor(s, r)

fun {f32, f32, f32}
  accel_wrap(f32 epsilon,
             {f32, f32, f32, f32, f32, f32, f32, f32, f32, f32} body_i,
             {f32, f32, f32, f32, f32, f32, f32, f32, f32, f32} body_j) =
  let {xi, yi, zi, mi, _, _, _, _, _, _} = body_i
  let {xj, yj, zj, mj, _, _, _, _, _, _} = body_j
  let pi = {xi, yi, zi}
  let pj = {xj, yj, zj}
  in accel(epsilon, pi, mi, pj, mj)
  
fun {f32, f32, f32}
  move(f32 epsilon,
       [{f32, f32, f32, f32, f32, f32, f32, f32, f32, f32}] bodies,
       {f32, f32, f32, f32, f32, f32, f32, f32, f32, f32} body) =
  let accels = map(fn {f32, f32, f32} ({f32, f32, f32, f32, f32, f32, f32, f32, f32, f32} body_other) =>
                     accel_wrap(epsilon, body, body_other),
                   bodies)
  in reduceComm(vec_add, {0f32, 0f32, 0f32}, accels)

fun [{f32, f32, f32}]
  calc_accels(f32 epsilon,
              [{f32, f32, f32, f32, f32, f32, f32, f32, f32, f32}] bodies) =
  map(move(epsilon, bodies), bodies)

fun {f32, f32, f32, f32, f32, f32, f32, f32, f32, f32}
  advance_body(f32 time_step,
               {f32, f32, f32, f32, f32, f32, f32, f32, f32, f32} body) =
  let {xp, yp, zp, mass, xv, yv, zv, xa, ya, za} = body
  let pos = {xp, yp, zp}
  let vel = {xv, yv, zv}
  let acc = {xa, ya, za}
  let pos' = vec_add(pos, vec_mult_factor(time_step, vel))
  let vel' = vec_add(vel, vec_mult_factor(time_step, acc))
  let {xp', yp', zp'} = pos'
  let {xv', yv', zv'} = vel'
  in {xp', yp', zp', mass, xv', yv', zv', xa, ya, za}
  
fun {f32, f32, f32, f32, f32, f32, f32, f32, f32, f32}
  advance_body_wrap(f32 time_step,
                    {f32, f32, f32, f32, f32, f32, f32, f32, f32, f32} body,
                    {f32, f32, f32} accel) =
  let {xp, yp, zp, m, xv, yv, zv, _, _, _} = body
  let accel' = vec_mult_factor(m, accel)
  let {xa', ya', za'} = accel'
  let body' = {xp, yp, zp, m, xv, yv, zv, xa', ya', za'}
  in advance_body(time_step, body')
  
fun [{f32, f32, f32, f32, f32, f32, f32, f32, f32, f32}, n]
  advance_bodies(f32 epsilon,
                 f32 time_step,
                 [{f32, f32, f32, f32, f32, f32, f32, f32, f32, f32}, n] bodies) =
  let accels = calc_accels(epsilon, bodies)
  in zipWith(advance_body_wrap(time_step), bodies, accels)

fun [{f32, f32, f32, f32, f32, f32, f32, f32, f32, f32}, n]
  advance_bodies_steps(i32 n_steps,
                       f32 epsilon,
                       f32 time_step,
                       [{f32, f32, f32, f32, f32, f32, f32, f32, f32, f32}, n] bodies) =
  loop (bodies) = for i < n_steps do
    advance_bodies(epsilon, time_step, bodies)
  in bodies

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
  let bodies = zip(xps, yps, zps, ms, xvs, yvs, zvs, xas, yas, zas)
  let bodies' = advance_bodies_steps(n_steps, epsilon, time_step, bodies)
  in unzip(bodies')

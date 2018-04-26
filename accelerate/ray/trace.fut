-- ==
-- input {
--   800 600
--
--   100
--
--   50f32 -100f32 -700f32
--
--   4
--
--   0f32
-- }

import "/futlib/math"
import "/futlib/colour"
import "types"
import "objects"
import "intersection"
import "lights"

let point_of_index (sizeX: i32) (sizeY: i32) ((x,y): (i32,i32)): (f32,f32) =
  let fsizeX = r32 sizeX
  let fsizeY = r32 sizeY
  let fsizeX2 = fsizeX / 2.0
  let fsizeY2 = fsizeY / 2.0
  let midX = sizeX / 2
  let midY = sizeY / 2
  in (r32 (x-midX) / fsizeX2,
      r32 (y-midY) / fsizeY2)

let cast_view_rays (sizeX: i32) (sizeY: i32) (fov: i32) (eye_pos: position)
                 : [sizeX][sizeY]direction =
  let sizeX' = r32 sizeX
  let sizeY' = r32 sizeY
  let aspect = sizeX' / sizeY'
  let fov' = r32 fov
  let fovX = fov' * aspect
  let fovY = fov'
  let cast (x: i32) (y: i32) =
    (let (x',y') = point_of_index sizeX sizeY (x,y)
     in vec3.normalise vec3.({x=x'*fovX, y=y'*fovY, z=0.0} - eye_pos))
  in map (\x -> map (cast x) (iota sizeY)) (iota sizeX)

let hit_sphere (sph: sphere) (dist: f32) (orig: position) (dir: direction)
             : (position, direction, argb.colour, f32) =
  let point = orig vec3.+ vec3.scale dist dir
  let normal = sphere_normal sph point
  let colour = sph.colour
  let shine = sph.shine
  in (point, normal, colour, shine)

let hit_plane (pln: plane) (dist: f32) (orig: position) (dir: direction)
            : (position, direction, argb.colour, f32) =
  let point = orig vec3.+ vec3.scale dist dir
  let normal = pln.normal
  let colour = checkers point
  let shine = pln.shine
  in (point, normal, colour, shine)

-- Cast a single ray into the scene.  In the Accelerate formulation,
-- this is done by a bounded loop that is fully unrolled at
-- compile-time.  Since we don't have a powerful meta-language, we
-- have to actually implement the loop.  I had to mangle things a
-- little bit since the original formulation is recursive, so I'm not
-- sure the optics are exactly the same.  We are also able to escape
-- early, in case a ray fails to collide with anyting.
let trace_ray (limit: i32) ({spheres,planes}: objects) (lights: lights)
              (ambient: argb.colour) (orig_point: position) (orig_dir: direction) =
  let dummy_sphere = {position={x=0.0, y=0.0, z=0.0},
                      colour=argb.black,
                      shine=0.0,
                      radius=0.0}: sphere
  let dummy_plane = {position={x=0.0, y=0.0, z=0.0},
                     normal={x=0.0, y=0.0, z=0.0},
                     colour=argb.black,
                     shine=0.0}: plane
  let (_, refl_colour,_,_,_) =
    loop (i, refl_colour, point, dir, visibility) =
         (0, argb.black, orig_point, orig_dir, 1.0) while i < limit do
    let (hit_s, dist_s, s) = cast_ray_sphere spheres dummy_sphere point dir
    let (hit_p, dist_p, p) = cast_ray_plane planes dummy_plane point dir
    in if !(hit_s || hit_p) then (limit, refl_colour, point, dir, visibility) else
    -- Ray hit an object.
    let next_s = hit_sphere s dist_s point dir
    let next_p = hit_plane p dist_p point dir

    -- Does the sphere or plane count?
    let (point, normal, colour, shine) =
      if dist_s < dist_p then next_s else next_p

    -- Determine reflection angle.
    let newdir = dir vec3.- vec3.scale (2.0 * vec3.dot normal dir) normal

    -- Determine direct lighting at this point.
    let direct = apply_lights {spheres=spheres,planes=planes} lights point normal

    -- Total lighting is direct plus ambient
    let lighting = argb.add direct ambient

    let light_in = argb.scale (argb.mult lighting colour) (1.0-shine)

    let light_out = argb.mix (1.0-visibility) refl_colour visibility light_in

    in (i+1,
        light_out,
        point,
        newdir,
        visibility * shine)
  in refl_colour

let make_objects (time: f32): objects =
  {spheres = [{position={x= 40.0 * f32.sin time, y=80.0, z=0.0},
               radius=20.0,
               colour=argb.from_rgba 1.0 0.3 1.0 1.0,
               shine=0.4},

              {position={x= 200.0 * f32.sin time,
                         y= -40.0 * f32.sin (time + f32.pi/2.0),
                         z= 200.0 * f32.cos time},
               radius=100.0,
               colour=argb.from_rgba 0.4 0.4 1.0 1.0,
               shine=0.8},

              {position={x= -200.0 * f32.sin time,
                         y= -40.0 * f32.sin (time - f32.pi/2.0),
                         z= -200.0 * f32.cos time},
               radius=100.0,
               colour=argb.from_rgba 0.4 0.4 1.0 1.0,
               shine=0.5},

              {position={x=0.0, y= -150.0, z= -100.0},
               radius=50.0,
               colour=argb.from_rgba 1.0 1.0 1.0 1.0,
               shine=0.8}],

   planes = [{position={x=0.0, y=100.0, z=0.0},
              normal={x= 0.0, y= -0.9805807, z= -0.19611613},
              colour=argb.white,
              shine=0.2}]}

let main (sizeX: i32) (sizeY: i32) (fov: i32) (eyeX: f32) (eyeY: f32) (eyeZ: f32) (limit: i32) (time: f32) =
  let lights: lights = {lights=[{position={x= 300.0, y= -300.0, z= -100.0},
                                 colour=argb.red}]}

  let objects: objects = make_objects time
  let ambient = argb.from_rgba 0.3 0.3 0.3 1.0
  let eye_pos = {x=eyeX, y=eyeY, z=eyeZ}
  let eye_rays = cast_view_rays sizeX sizeY fov eye_pos
  in map (\rays -> map (trace_ray limit objects lights ambient eye_pos) rays) eye_rays

import "/futlib/math"
import "types"
import "objects"

-- Find the nearest point of intersection of a ray by brute force.

let cast_ray [n] 'object
             (distance_to: object -> position -> direction -> (bool, f32))
             (objects: [n]object) (dummy: object) (orig: position) (dir: direction)
           : (bool, f32, object) =
    loop (hit, closest_hit_dist, closest_hit_obj) =
         (false, f32.inf, dummy) for i < n do
      (let (new_hit, dist) = distance_to objects[i] orig dir
       in if new_hit && dist < closest_hit_dist
          then (new_hit, dist, objects[i])
          else (hit, closest_hit_dist, closest_hit_obj))

let cast_ray_sphere = cast_ray sphere.distance_to
let cast_ray_plane = cast_ray plane.distance_to

-- Look for an object closer than a given minimum distance; stopping
-- as soon as we find an intersection.
let check_ray [n] 'object
              (distance_to: object -> position -> direction -> (bool, f32))
              (objects: [n]object) (orig: position) (dir: direction) (dist: f32)
            : bool =
  let (hit, _) =
    loop (hit,i) = (false,0) while !hit && i < n do
      let (new_hit,dist') = distance_to (unsafe objects[i]) orig dir
      in if new_hit && dist' < dist then (true, i) else (false, i+1)
  in hit

let check_ray_sphere = check_ray sphere.distance_to
let check_ray_plane = check_ray plane.distance_to

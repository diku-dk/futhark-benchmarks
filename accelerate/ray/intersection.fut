import "/futlib/math"
import "types"
import "objects"

-- Find the nearest point of intersection of a ray by brute force.  As
-- a parametric module because we have no higher-order functions in
-- Futhark.

module cast_ray(O: object) = {
  let cast_ray [n] (objects: [n]O.object) (dummy: O.object) (orig: position) (dir: direction): (bool, f32, O.object) =
    loop (hit, closest_hit_dist, closest_hit_obj) =
         (false, f32.inf, dummy) for i < n do
      (let (new_hit, dist) = O.distance_to objects[i] orig dir
       in if new_hit && dist < closest_hit_dist
          then (new_hit, dist, objects[i])
          else (hit, closest_hit_dist, closest_hit_obj))
}

module cast_ray_sphere = cast_ray sphere
module cast_ray_plane = cast_ray plane

-- Look for an object closer than a given minimum distance; stopping
-- as soon as we find an intersection.
module check_ray(O: object) = {
  let check_ray [n] (objects: [n]O.object) (orig: position) (dir: direction) (dist: f32): bool =
    let (hit, _) =
      loop (hit,i) = (false,0) while !hit && i < n do
        let (new_hit,dist') = O.distance_to (unsafe objects[i]) orig dir
        in if new_hit && dist' < dist then (true, i) else (false, i+1)
    in hit
}

module check_ray_sphere = check_ray sphere
module check_ray_plane = check_ray plane

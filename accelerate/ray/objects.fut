import "/futlib/colour"
import "/futlib/math"
import "types"

default (f32)

module type object = {
  type object
  val distance_to: object -> position -> direction -> (bool, f32)
}

type sphere = {position: position,
               colour: argb.colour,
               shine: f32,
               radius: f32}

module sphere: object with object = sphere = {
  type object = sphere

  let distance_to (sphere: sphere)
                  (origin: position)
                  (direction: direction): (bool, f32) =
    let pos = #position sphere
    let radius = #radius sphere
    let p = origin vec3.+ (vec3.scale (vec3.dot (pos vec3.- origin) direction) direction)
    let d_cp = vec3.norm (p vec3.- pos)
    let sep = p vec3.- origin
    let miss = d_cp >= radius || vec3.dot sep direction <= 0.
    in if miss
       then (false, f32.inf)
       else (true, vec3.norm sep - f32.sqrt (radius*radius - d_cp * d_cp))
}

let sphere_normal (sphere: sphere) (point: position): direction =
  vec3.normalise (point vec3.- #position sphere)

type plane = {position: position,
              normal: direction,
              colour: argb.colour,
              shine: f32}

type plane_check = plane

module plane: object with object = plane = {
  type object = plane

  let distance_to (plane: plane)
                  (origin: position)
                  (direction: direction): (bool, f32) =
    let pos = #position plane
    let normal = #normal plane
    let theta = vec3.dot direction normal
    in if theta >= 0.
       then (false, f32.inf)
       else (true, vec3.dot (pos vec3.- origin) normal / theta)
}

let checkers ((x,_,z): position): argb.colour =
  let v1 = i32 (x/100.0) % 2
  let v2 = i32 (z/100.0) % 2
  let v3 = i32 (x < 0.0)
  let v4 = i32 (z < 0.0)
  in if v1 ^ v2 ^ v3 ^ v4 == 1
     then argb.from_rgba 1.0 1.0 1.0 1.0
     else argb.from_rgba 0.4 0.4 0.4 1.0

type objects = {spheres: []sphere, planes: []plane}

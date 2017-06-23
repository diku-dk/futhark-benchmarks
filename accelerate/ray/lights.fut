import "/futlib/math"
import "/futlib/colour"
import "types"
import "objects"
import "intersection"

default (f32)

type light = {position: position,
              colour: argb.colour}

type lights = {lights: []light}

let apply_light ({spheres,planes}: objects) (point: position) (normal: direction)
                (light: light): argb.colour =
  let lp_p = #position light vec3.- point
  let dist = vec3.norm lp_p
  let dir = vec3.scale (1.0 / dist) lp_p

  let mag = vec3.dot normal dir / (dist * dist)
  let (r,g,b,_) = argb.to_rgba (#colour light)
  let refl = argb.from_rgba (r*mag) (g*mag) (b*mag) 1.0
  in if check_ray_sphere.check_ray spheres point dir dist ||
        check_ray_plane.check_ray planes point dir dist
     then argb.black else refl

let apply_lights [num_lights]
                 (objects: objects) ({lights:[num_lights]light}: lights)
                 (point: position) (normal: direction)
                : argb.colour =
  loop (c = argb.black) for i < num_lights do
    argb.mix 1.0 c 1.0 (apply_light objects point normal lights[i])

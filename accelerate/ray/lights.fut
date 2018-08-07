import "lib/github.com/athas/matte/colour"
import "types"
import "objects"
import "intersection"

type light = {position: position,
              colour: argb.colour}

type lights = {lights: []light}

let apply_light ({spheres,planes}: objects) (point: position) (normal: direction)
                (light: light): argb.colour =
  let lp_p = light.position vec3.- point
  let dist = vec3.norm lp_p
  let dir = vec3.scale (1.0 / dist) lp_p

  in if check_ray_sphere spheres point dir dist ||
        check_ray_plane planes point dir dist
     then argb.black
     else let mag = vec3.dot normal dir
          let (r,g,b,_) = argb.to_rgba light.colour
          let refl = argb.from_rgba (r*mag) (g*mag) (b*mag) 1.0
          in refl

let apply_lights [num_lights]
                 (objects: objects) ({lights:[num_lights]light}: lights)
                 (point: position) (normal: direction)
                : argb.colour =
  let mix c l = argb.add_linear c (apply_light objects point normal l)
  in foldl mix argb.black lights

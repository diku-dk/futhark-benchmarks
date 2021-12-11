import "lib/github.com/athas/vector/vspace"

module vec3 = mk_vspace_3d f32
type vec3 = vec3.vector

-- A convenient alias so we don't have to indicate the fields all the
-- time.
def vec (x, y, z) : vec3 = {x,y,z}

-- | Axis-aligned bounding box.
type aabb = { min: vec3, max: vec3 }

def surrounding_box (box0: aabb) (box1: aabb) : aabb =
  let small = vec(f32.min box0.min.x box1.min.x,
                  f32.min box0.min.y box1.min.y,
                  f32.min box0.min.z box1.min.z)
  let big = vec(f32.max box0.max.x box1.max.x,
                f32.max box0.max.y box1.max.y,
                f32.max box0.max.z box1.max.z)
  in {min = small, max = big}

def aabb_center ({min, max}: aabb) =
  {x=min.x + (max.x - min.x),
   y=min.y + (max.y - min.y),
   z=min.z + (max.z - min.z)}

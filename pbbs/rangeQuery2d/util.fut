type point = [2]f64
type rectangle = [4]f64

-- Build rectangles from points
def build_rectangle [k] (ps: [2 * k]point) : [k]rectangle =
  map (\i ->
         let p1 = ps[2 * i]
         let p2 = ps[2 * i + 1]
         in [p1[0], p1[1], p2[0], p2[1]])
      (iota k)

-- Split input points into rectangles and points
def split_input [n] (p: [n]point) : ([]point, []rectangle) =
  let rectangles_points = take (2 * (n / 3)) p
  let rectangles = build_rectangle rectangles_points
  let points = drop (2 * (n / 3)) p
  in (points, rectangles)

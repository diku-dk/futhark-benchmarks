-- Torben Mogensen's four-state Game of Life.

import "genlife"
import "futlib/colour"

module quad_rules: vis_rules = {
  type cell = i8

  fun value (c: cell) = i32 c

  val weights = [[1,1,1],
                 [1,1,1],
                 [1,1,1]]

  fun step (_c: cell) (neighbours: i32) =
    let t = [0i8,0i8,0i8,2i8,2i8,3i8,3i8,
             1i8,1i8,1i8,1i8,1i8,0i8,0i8,
             2i8,0i8,2i8,2i8,2i8,2i8,2i8,
             0i8,2i8,1i8,2i8,3i8,3i8,3i8]
    in unsafe t[neighbours]

  fun init (b: bool) =
    if b then 1i8 else 0i8

  fun uninit (c: cell) =
    c != 1i8

  fun colour (c: cell) =
    let colours = [argb.green,
                   argb.black,
                   argb.red,
                   argb.blue]
    in unsafe colours[i32 c]
}

module quad = gen_life_vis(quad_rules)

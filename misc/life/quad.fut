-- Torben Mogensen's four-state Game of Life.

import "genlife"
import "fading"
import "futlib/colour"

module quad_rules: rules_and_visuals = {
  type cell = i8

  let value (c: cell) = i32 c

  let weights = [[1,1,1],
                 [1,1,1],
                 [1,1,1]]

  let step (_c: cell) (neighbours: i32) =
    let t = [0i8,0i8,0i8,2i8,2i8,3i8,3i8,
             1i8,1i8,1i8,1i8,1i8,0i8,0i8,
             2i8,0i8,2i8,2i8,2i8,2i8,2i8,
             0i8,2i8,1i8,2i8,3i8,3i8,3i8]
    in unsafe t[neighbours]

  let init (b: bool) =
    if b then 0i8 else 1i8

  let uninit (c: cell) =
    c != 1i8

  let colour (c: cell) =
    let colours = [argb.green,
                   argb.black,
                   argb.red,
                   argb.blue]
    in unsafe colours[i32 c]
}

module quad = gen_life_vis quad_rules quad_rules
module quad_fading = slow_fader quad_rules quad_rules

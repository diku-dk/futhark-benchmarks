-- Torben Mogensen's four-state Game of Life.

import "genlife"
import "fading"
import "lib/github.com/athas/matte/colour"

module quad_rules: rules_and_visuals = {
  type cell = i8

  def value (c: cell) = i32.i8 c

  def weights: [3][3]i32 = [[1,1,1],
                            [1,1,1],
                            [1,1,1]]

  def step (_c: cell) (neighbours: i32) =
    let t = [0i8,0i8,0i8,2i8,2i8,3i8,3i8,
             1i8,1i8,1i8,1i8,1i8,0i8,0i8,
             2i8,0i8,2i8,2i8,2i8,2i8,2i8,
             0i8,2i8,1i8,2i8,3i8,3i8,3i8]
    in #[unsafe] t[neighbours]

  def init (b: bool) =
    if b then 0i8 else 1i8

  def uninit (c: cell) =
    c != 1i8

  def colour (c: cell) =
    let colours = [argb.green,
                   argb.black,
                   argb.red,
                   argb.blue]
    in #[unsafe] colours[i32.i8 c]
}

module quad = gen_life_vis quad_rules quad_rules
module quad_fading = slow_fader quad_rules quad_rules

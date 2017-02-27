-- Usual Conway rules for Game of Life.

import "genlife"
import "fading"

import "futlib/colour"

module conway_rules: vis_rules with cell = bool = {
  type cell = bool

  fun value (b: cell) = if b then 1 else 0

  val weights = [[1,1,1],
                 [1,0,1],
                 [1,1,1]]

  fun step (alive: cell) (neighbours: i32) =
    neighbours >= 2 &&
    (neighbours == 3 || (alive && neighbours < 4))

  fun init (b: bool) = b

  fun uninit (c: cell) = c

  fun colour (b: cell) = if b
                         then argb.black
                         else argb.white
}

module conway = gen_life_vis conway_rules

module conway_fading = slow_fader conway_rules

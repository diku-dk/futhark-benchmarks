-- Usual Conway rules for Game of Life.

import "genlife"
import "fading"

import "/futlib/colour"

module conway_rules: rules with cell = bool = {
  type cell = bool

  let value (b: cell) = if b then 1 else 0

  let weights = [[1,1,1],
                 [1,0,1],
                 [1,1,1]]

  let step (alive: cell) (neighbours: i32) =
    neighbours >= 2 &&
    (neighbours == 3 || (alive && neighbours < 4))
}

module conway_visuals: visuals with cell = bool = {
  type cell = bool

  let init (b: bool) = b

  let uninit (c: cell) = c

  let colour (b: cell) = if b
                         then argb.black
                         else argb.white
}

module conway = gen_life_vis conway_rules conway_visuals

module conway_fading = slow_fader conway_rules conway_visuals

-- Usual Conway rules for Game of Life.

import "genlife"
import "fading"

import "futlib/colour"

module conway_rules: rules_and_vis with cell = bool = {
  type cell = bool

  fun cell_value (b: bool) = if b then 1 else 0

  val weights = [[1,1,1],
                 [1,0,1],
                 [1,1,1]]

  fun step (alive: bool) (neighbours: i32) =
    if neighbours < 2
    then false
    else if neighbours == 3 then true
    else if alive && neighbours < 4 then true
    else false

  fun init (b: bool) = b

  fun uninit (c: bool) = c

  fun colour (b: bool) = if b
                        then argb.black
                        else argb.white
}

module conway = gen_life_vis(conway_rules)

module conway_fading = gen_life_fading_vis({
  open conway_rules
  val dying_speed = 0.1f32
  fun dying_colour (_: cell) = argb.gray 0.6f32
  fun dead (b: bool) = !b
})

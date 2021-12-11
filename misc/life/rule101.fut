-- An adaptation of Rule 110.  This is a 1D automaton, but we can
-- model it as a 2D stencil by making the extra dimension time.
--
-- Rules from Wikipedia:
--
-- current pattern            111   110   101   100   011   010   001   000
-- new state for center cell  0       1     1     0     1     1     1     0

import "genlife"
import "genlife_1d"

import "lib/github.com/athas/matte/colour"

module rule101_rules: rules_and_visuals_1d = {
  type cell = bool

  def value (b: cell) = i32.bool b

  def weights: [3]i32 = [1,2,2]

  def step (_: cell) (neighbours: i32) =
    neighbours == 2 || neighbours == 3 || neighbours == 4

  def init (b: bool) = b

  def uninit (c: cell) = c

  def colour (b: cell) = if b
                         then argb.black
                         else argb.white
}

module rule101 = gen_life_vis_1d rule101_rules rule101_rules

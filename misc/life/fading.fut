-- Add history-based fading to any Game of Life ruleset.  Only affects
-- the visualisation; the underlying rules are the same.
--
-- When a cell goes "dead", it does not immediately switch to the
-- colour of dead cells.  Instead, it will switch to the "dying"
-- colour, and fade to the dead colour over some period of steps.

import "genlife"

import "futlib/colour"

module type fading = {
  type cell

  val dying_speed: f32

  val dying_colour: cell -> argb.colour

  -- Whether a cell is dead.
  val dead: cell -> bool
}

module fading_life =
  \(R: vis_rules) ->
  \(F: fading with cell = R.cell)
  : vis_game_of_life ->

  gen_life_vis {
-- The cell, and the amount of time it has been dead.
type cell = (R.cell, argb.colour, i32)

fun value ((c,_,_): cell) = R.value c

val weights = R.weights

fun step ((c,col,h): cell) (neighbours: i32) =
  let c' = R.step c neighbours
  let died = ! (F.dead c) && F.dead c'
  in (c',
      if died then F.dying_colour c else col,
      if died then 0                else h + 1)

fun init (b: bool) = (R.init b, argb.black, 10000)

fun uninit ((c,_,_): cell) = R.uninit c

fun colour ((c,col,h): cell) =
  let normal = R.colour c in
  if F.dead c
  then argb.mix 1f32 col (f32 h * F.dying_speed) normal
  else normal
}

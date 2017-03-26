-- Add history-based fading to any Game of Life ruleset.  Only affects
-- the visualisation; the underlying rules are the same.
--
-- When a cell goes "dead", it does not immediately switch to the
-- colour of dead cells.  Instead, it will switch to the "dying"
-- colour, and fade to the dead colour over some period of steps.

import "genlife"

import "futlib/colour"

module type fading = {
  val dying_speed: f32
}

module fading_life =
  \(F: fading) ->
  \(R: vis_rules)
  : vis_game_of_life ->

  gen_life_vis {
-- The cell, and the amount of time it has been dead.
type cell = (R.cell, argb.colour, i32)

let value ((c,_,_): cell) = R.value c

let weights = R.weights

let init (b: bool) = (R.init b, argb.black, 10000)

let uninit ((c,_,_): cell) = R.uninit c

let dead (c: R.cell): bool = R.uninit c == false

let step ((c,col,h): cell) (neighbours: i32) =
  let c' = R.step c neighbours
  let died = ! (dead c) && dead c'
  in (c',
      if died then R.colour c else col,
      if died then 0          else h + 1)

let colour ((c,col,h): cell) =
  let normal = R.colour c in
  if dead c
  then argb.mix 1f32 col (f32 h * F.dying_speed) normal
  else normal
}

module slow_fader: (vis_rules -> vis_game_of_life) = fading_life {
  let dying_speed = 0.1f32
}

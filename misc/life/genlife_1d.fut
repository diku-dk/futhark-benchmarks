-- A hack that models a 1D cellular automaton as a 2D automaton by
-- turning the extra dimension into time.

import "genlife"

module type rules_1d = {
  -- The abstract type of a cell.
  type cell

  -- Maps a cell to an integer.
  val value: cell -> i32

  -- Weights of cell values in a neighborhood.  These are
  -- multiplied with the cell values.  Position [0,0] is the
  -- leftmost cell.
  val weights: [3]i32

  -- Updating a single cell.  Called with the current cell and the sum
  -- of its neighbourhood.
  val step: cell -> i32 -> cell
}

module type rules_and_visuals_1d = {
  include rules_1d
  include visuals with cell = cell
}

module gen_life_1d(R: rules_1d): game_of_life with cell = R.cell = {
  type cell = R.cell

  let sum_of_neighbours (n:  cell) (c: cell) (s:  cell): i32 =
    (R.value n * R.weights[0] +
     R.value c * R.weights[1] +
     R.value s * R.weights[2])

  let advance(c: [#m]cell): [m]cell =
    let ns  = rotate (-1) c
    let ss  = rotate   1  c
    let neighbours = map sum_of_neighbours ns c ss
    in map R.step c neighbours

  -- Only the last column matters.  Note that the world is stored
  -- column-major.
  let step(world: [#n][#m]cell): [n][m]cell =
    let world' = rotate 1 world
    in map (\c i -> if i == n-1 then advance world[n-1] else c) world' (iota n)
}

module gen_life_vis_1d (R: rules_1d) (V: visuals with cell = R.cell): vis_game_of_life = {
  open (gen_life_1d R)
  open (gen_visualisation V)
}

-- Generic facilities for constructing your very own Game of Life.

import "lib/github.com/athas/matte/colour"

module type rules = {
  -- The abstract type of a cell.
  type cell

  -- Maps a cell to an integer.
  val value: cell -> i32

  -- Weights of cell values in a 3x3 neighborhood.  These are
  -- multiplied with the cell values.  Position [0,0] is the
  -- upper-left corner and this array is assumed row-major.
  val weights: [3][3]i32

  -- Updating a single cell.  Called with the current cell and the sum
  -- of its neighbourhood.
  val step: cell -> i32 -> cell
}

module type game_of_life = {
  type cell

  -- Run an iteration of the Game of Life.
  val step [n][m]: [n][m]cell -> [n][m]cell
}

module gen_life(R: rules): game_of_life with cell = R.cell = {
  type cell = R.cell

  def sum_of_neighbours (nw: cell, n: cell, ne: cell)
                        (w:  cell, c: cell, e:  cell)
                        (sw: cell, s: cell, se: cell): i32 =
    (R.value nw * R.weights[0,0] +
     R.value n  * R.weights[0,1] +
     R.value ne * R.weights[0,2]) +

    (R.value w * R.weights[1,0] +
     R.value c * R.weights[1,1] +
     R.value e * R.weights[1,2]) +

    (R.value sw * R.weights[2,0] +
     R.value s  * R.weights[2,1] +
     R.value se * R.weights[2,2])

  -- Note that the world is stored column-major.
  def all_neighbour_sums [n][m] (world: [n][m]cell): [n][m]i32 =
    let ns  = map (rotate (-1)) world
    let ss  = map (rotate   1)  world
    let ws  = rotate      (-1)  world
    let es  = rotate        1   world
    let nws = map (rotate (-1)) ws
    let nes = map (rotate (-1)) es
    let sws = map (rotate   1)  ws
    let ses = map (rotate   1)  es
    in map3 (\(nws_r, ns_r, nes_r) (ws_r, world_r, es_r) (sws_r, ss_r, ses_r) ->
             map3 sum_of_neighbours
             (zip3 nws_r ns_r nes_r) (zip3 ws_r world_r es_r) (zip3 sws_r ss_r ses_r))
            (zip3 nws ns nes) (zip3 ws world es) (zip3 sws ss ses)

  def step [n][m] (world: [n][m]cell): [n][m]cell =
    let all_sums = all_neighbour_sums world
    in map2 (\world_r all_sums_r -> map2 R.step world_r all_sums_r) world all_sums
}

module type visuals = {
  type cell

  -- Initialise cell from boolean.
  val init: bool -> cell

  -- Turn a cell back into a boolean.
  val uninit: cell -> bool

  -- Visualise one cell as a pixel colour.
  val colour: cell -> argb.colour
}

module type visualisation = {
  type cell

  val init [n][m]: [n][m]bool -> [n][m]cell
  val uninit [n][m]: [n][m]cell -> [n][m]bool

  -- Render the game world in ARGB format.  Alpha value is probably
  -- ignored.
  val render [n][m]: [n][m]cell -> [n][m]argb.colour
}

module gen_visualisation (V: visuals) : visualisation with cell = V.cell = {
  type cell = V.cell

  def init [n][m] (world: [n][m]bool): [n][m]cell =
    map (\row -> map V.init row) world

  def uninit [n][m] (world: [n][m]cell): [n][m]bool =
    map (\row -> map V.uninit row) world

  def render [n][m] (world: [n][m]cell): [n][m]argb.colour =
    map (\ages -> map V.colour ages) world
}

module type rules_and_visuals = {
  include rules
  include visuals with cell = cell -- cell type from game_of_life
}

-- A Game of Life that can also be initialised randomly and
-- visualised.
module type vis_game_of_life = {
  include game_of_life
  include visualisation with cell = cell -- cell type from game_of_life
}

module gen_life_vis(R: rules) (V: visuals with cell = R.cell) :
    vis_game_of_life with cell = R.cell = {
  open (gen_life R)
  open (gen_visualisation V)
}

-- Generic facilities for constructing your very own Game of Life.

import "futlib/colour"

module type rules = {
  -- The abstract type of a cell.
  type cell

  -- Maps a cell to an integer.
  val value: cell -> i32

  -- Weights of cell values in a 3x3 neighborhood.  These are
  -- multiplied with the cell values.  Position [0,0] is the
  -- upper-left corner.
  val weights: [3][3]i32

  -- Updating a single cell.  Called with the current cell and the sum
  -- of its neighbourhood.
  val step: cell -> i32 -> cell
}

module type game_of_life = {
  type cell

  -- Run an iteration of the Game of Life.
  val step: [][]cell -> [][]cell
}

module gen_life(R: rules): game_of_life with cell = R.cell = {
  type cell = R.cell

  let sum_of_neighbours (nw: cell) (n: cell) (ne: cell)
                        (w:  cell) (c: cell) (e:  cell)
                        (sw: cell) (s: cell) (se: cell): i32 =
    (R.value nw * R.weights[0,0] +
     R.value n  * R.weights[0,1] +
     R.value ne * R.weights[0,2]) +

    (R.value w * R.weights[1,0] +
     R.value c * R.weights[1,1] +
     R.value e * R.weights[1,2]) +

    (R.value sw * R.weights[2,0] +
     R.value s  * R.weights[2,1] +
     R.value se * R.weights[2,2])

  let all_neighbour_sums(world: [n][m]cell): [n][m]i32 =
    let ns  = rotate   (-1) world
    let ss  = rotate     1  world
    let ws  = rotate@1 (-1) world
    let es  = rotate@1   1  world
    let nws = rotate   (-1) ws
    let nes = rotate   (-1) es
    let sws = rotate     1  ws
    let ses = rotate     1  es
    in map (\nws_r ns_r nes_r ws_r world_r es_r sws_r ss_r ses_r ->
            map sum_of_neighbours
            nws_r ns_r nes_r ws_r world_r es_r sws_r ss_r ses_r)
           nws ns nes ws world es sws ss ses

  let step(world: [n][m]cell): [n][m]cell =
    let all_sums = all_neighbour_sums world
    in map (\world_r all_sums_r -> map R.step world_r all_sums_r) world all_sums
}

module type vis_rules = {
  include rules

  -- Initialise cell from boolean.
  val init: bool -> cell

  -- Turn a cell back into a boolean.
  val uninit: cell -> bool

  -- Visualise one cell as a pixel colour.
  val colour: cell -> argb.colour
}

-- A Game of Life that can also be initialised randomly and
-- visualised.
module type vis_game_of_life = {
  include game_of_life

  val init: [][]bool -> [][]cell
  val uninit: [][]cell -> [][]bool

  -- Render the game world in ARGB format.  Alpha value is probably
  -- ignored.
  val render: [][]cell -> [][]i32
}

module gen_life_vis(R: vis_rules): vis_game_of_life with cell = R.cell = {
  open (gen_life R)

  let init(world: [n][m]bool): [n][m]cell =
    map (\row -> map R.init row) world

  let uninit(world: [n][m]cell): [n][m]bool =
    map (\row -> map R.uninit row) world

  let render(world: [n][m]cell): [n][m]i32 =
    map (\ages -> map R.colour ages) world
}

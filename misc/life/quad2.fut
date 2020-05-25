-- Torben Mogensen's alternative four-state Game of Life.
--
-- This is a variant where only orthogonal neighbours are considered.

import "genlife"
import "fading"
import "lib/github.com/athas/matte/colour"

module quad2_rules: rules_and_visuals = {
  type cell = i8

  let value (c: cell) = i32.i8 c

  let weights: [3][3]i32 = [[0,1,0],
                            [1,1,1],
                            [0,1,0]]

  let step (_c: cell) (neighbours: i32) =
    let t = [0i8, 1i8, 1i8, 0i8,
             0i8, 1i8, 1i8, 1i8,
             2i8, 2i8, 2i8, 3i8,
             3i8, 2i8, 2i8, 3i8]
    in #[unsafe] t[neighbours]

  let init (b: bool) =
    if b then 1i8 else 0i8

  let uninit (c: cell) =
    c == 1i8

  let colour (c: cell) =
    let colours = [argb.blue,
                   argb.green,
                   argb.red,
                   argb.yellow]
    in #[unsafe] colours[i32.i8 c]
}

module quad2 = gen_life_vis quad2_rules quad2_rules

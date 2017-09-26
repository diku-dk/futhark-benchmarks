-- Simple game of life implementation with a donut world.
--
-- ==
-- compiled input {
--   [[0, 0, 0, 0, 0],
--    [0, 0, 1, 0, 0],
--    [0, 0, 0, 1, 0],
--    [0, 1, 1, 1, 0],
--    [0, 0, 0, 0, 0]]
--   10000
--   4
--   }

import "genlife"
import "conway"
import "quad"
import "quad2"
import "rule101"

entry conway_init [n][m] (bs: [n][m]bool) =
 conway.init bs
entry conway_steps [n][m] (k: i32) (world: [n][m]conway.cell) =
 loop (world) for _i < k do conway.step world
entry conway_render [n][m] (world: [n][m]conway.cell) =
 conway.render world
entry conway_uninit [n][m] (world: [n][m]conway.cell) =
 conway.uninit world

entry conway_fading_init [n][m] (bs: [n][m]bool) =
 conway_fading.init bs
entry conway_fading_steps [n][m] (k: i32) (world: [n][m]conway_fading.cell) =
 loop (world) for _i < k do conway_fading.step world
entry conway_fading_render [n][m] (world: [n][m]conway_fading.cell) =
 conway_fading.render world
entry conway_fading_uninit [n][m] (world: [n][m]conway_fading.cell) =
 conway_fading.uninit world

entry quad_init [n][m] (bs: [n][m]bool) =
 quad.init bs
entry quad_steps [n][m] (k: i32) (world: [n][m]quad.cell) =
 loop (world) for _i < k do quad.step world
entry quad_render [n][m] (world: [n][m]quad.cell) =
 quad.render world
entry quad_uninit [n][m] (world: [n][m]quad.cell) =
 quad.uninit world

entry quad_fading_init [n][m] (bs: [n][m]bool) =
 quad_fading.init bs
entry quad_fading_steps [n][m] (k: i32) (world: [n][m]quad_fading.cell) =
 loop (world) for _i < k do quad_fading.step world
entry quad_fading_render [n][m] (world: [n][m]quad_fading.cell) =
 quad_fading.render world
entry quad_fading_uninit [n][m] (world: [n][m]quad_fading.cell) =
 quad_fading.uninit world

entry quad2_init [n][m] (bs: [n][m]bool) =
 quad2.init bs
entry quad2_steps [n][m] (k: i32) (world: [n][m]quad2.cell) =
 loop (world) for _i < k do quad2.step world
entry quad2_render [n][m] (world: [n][m]quad2.cell) =
 quad2.render world
entry quad2_uninit [n][m] (world: [n][m]quad2.cell) =
 quad2.uninit world

entry rule101_init [n][m] (bs: [n][m]bool) =
 rule101.init bs
entry rule101_steps [n][m] (k: i32) (world: [n][m]rule101.cell) =
 loop (world) for _i < k do rule101.step world
entry rule101_render [n][m] (world: [n][m]rule101.cell) =
 rule101.render world
entry rule101_uninit [n][m] (world: [n][m]rule101.cell) =
 rule101.uninit world

-- Just a simple test to force the program to be compiled.
let main [n][m] (base_pattern: [n][m]i32) (repeats: i32) (k: i32) =
  let pattern = reshape (n*k,m*k)
                (replicate k
                 (map (\row -> reshape (k*m) (replicate k (map bool row)))
                      base_pattern))
  let world =
    loop world = conway.init pattern for _i < k do conway.step world
  in reduce (+) 0 (map i32 (reshape (k*n*k*m) world))

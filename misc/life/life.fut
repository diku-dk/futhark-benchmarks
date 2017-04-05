-- Simple game of life implementation with a donut world.

import "genlife"
import "conway"
import "quad"
import "quad2"
import "rule101"

entry conway_init (bs: [#n][#m]bool) =
 conway.init bs
entry conway_steps (k: i32) (world: [#n][#m]conway.cell) =
 loop (world) = for _i < k do conway.step world in world
entry conway_render (world: [#n][#m]conway.cell) =
 conway.render world
entry conway_uninit (world: [#n][#m]conway.cell) =
 conway.uninit world

entry conway_fading_init (bs: [#n][#m]bool) =
 conway_fading.init bs
entry conway_fading_steps (k: i32) (world: [#n][#m]conway_fading.cell) =
 loop (world) = for _i < k do conway_fading.step world in world
entry conway_fading_render (world: [#n][#m]conway_fading.cell) =
 conway_fading.render world
entry conway_fading_uninit (world: [#n][#m]conway_fading.cell) =
 conway_fading.uninit world

entry quad_init (bs: [#n][#m]bool) =
 quad.init bs
entry quad_steps (k: i32) (world: [#n][#m]quad.cell) =
 loop (world) = for _i < k do quad.step world in world
entry quad_render (world: [#n][#m]quad.cell) =
 quad.render world
entry quad_uninit (world: [#n][#m]quad.cell) =
 quad.uninit world

entry quad_fading_init (bs: [#n][#m]bool) =
 quad_fading.init bs
entry quad_fading_steps (k: i32) (world: [#n][#m]quad_fading.cell) =
 loop (world) = for _i < k do quad_fading.step world in world
entry quad_fading_render (world: [#n][#m]quad_fading.cell) =
 quad_fading.render world
entry quad_fading_uninit (world: [#n][#m]quad_fading.cell) =
 quad_fading.uninit world

entry quad2_init (bs: [#n][#m]bool) =
 quad2.init bs
entry quad2_steps (k: i32) (world: [#n][#m]quad2.cell) =
 loop (world) = for _i < k do quad2.step world in world
entry quad2_render (world: [#n][#m]quad2.cell) =
 quad2.render world
entry quad2_uninit (world: [#n][#m]quad2.cell) =
 quad2.uninit world

entry rule101_init (bs: [#n][#m]bool) =
 rule101.init bs
entry rule101_steps (k: i32) (world: [#n][#m]rule101.cell) =
 loop (world) = for _i < k do rule101.step world in world
entry rule101_render (world: [#n][#m]rule101.cell) =
 rule101.render world
entry rule101_uninit (world: [#n][#m]rule101.cell) =
 rule101.uninit world

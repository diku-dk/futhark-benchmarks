-- Simple game of life implementation with a donut world.

import "genlife"
import "conway"
import "quad"
import "quad2"

entry conway_init (bs: [n][m]bool) =
 conway.init bs
entry conway_steps (k: i32) (world: [n][m]conway.cell) =
 conway.steps k world
entry conway_render (world: [n][m]conway.cell) =
 conway.render world
entry conway_uninit (world: [n][m]conway.cell) =
 conway.uninit world

entry conway_fading_init (bs: [n][m]bool) =
 conway_fading.init bs
entry conway_fading_steps (k: i32) (world: [n][m]conway_fading.cell) =
 conway_fading.steps k world
entry conway_fading_render (world: [n][m]conway_fading.cell) =
 conway_fading.render world
entry conway_fading_uninit (world: [n][m]conway_fading.cell) =
 conway_fading.uninit world

entry quad_init (bs: [n][m]bool) =
 quad.init bs
entry quad_steps (k: i32) (world: [n][m]quad.cell) =
 quad.steps k world
entry quad_render (world: [n][m]quad.cell) =
 quad.render world
entry quad_uninit (world: [n][m]quad.cell) =
 quad.uninit world

entry quad2_init (bs: [n][m]bool) =
 quad2.init bs
entry quad2_steps (k: i32) (world: [n][m]quad2.cell) =
 quad2.steps k world
entry quad2_render (world: [n][m]quad2.cell) =
 quad2.render world
entry quad2_uninit (world: [n][m]quad2.cell) =
 quad2.uninit world

-- | ignore

-- This file exists as a wrapper that defines entry points in the
-- specific form that liblys.c requires.  It is copied into place and
-- modified by the rules in common.mk.

module m = import "lys"

type state = m.lys.state

entry init (seed: i32) (h: i32) (w: i32): state = m.lys.init seed h w

entry resize (h: i32) (w: i32) (s: state): state = m.lys.resize h w s

entry key (e: i32) (key: i32) (s: state): state =
  let e' = if e == 0 then #keydown else #keyup
  in m.lys.key e' key s

entry mouse (mstate: i32) (x: i32) (y: i32) (s: state): state =
  m.lys.mouse mstate x y s

entry wheel (x: i32) (y: i32) (s: state): state =
  m.lys.wheel x y s

entry step (td: f32) (s: state): state = m.lys.step td s

entry render (s: state) = m.lys.render s

entry text_colour (s: state): m.argb.colour =
  m.lys.text_colour s

entry text_format: []u8 = m.lys.text_format

entry text_content (render_duration: f32) (s: state): m.lys.text_content =
  m.lys.text_content render_duration s

entry grab_mouse: bool =
  m.lys.grab_mouse

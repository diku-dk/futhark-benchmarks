-- | ignore

-- This file exists as a wrapper that defines entry points in the
-- specific form that liblys.c requires.  It is copied into place and
-- modified by the rules in common.mk.

module m = import "lys"

type state = m.lys.state

entry init (h: i32) (w: i32): state = m.lys.init h w

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

entry text (render_duration: f32) (s: state): m.printf_input_cs =
  let inps = m.lys.text render_duration s
  in unzip5 (map (\(s, args, colour) ->
                    let (ts, vs) = unzip (map (\(t, v) ->
                                                 let t' = match t
                                                          case #placeholder -> 0
                                                          case #f32 -> 1
                                                          case #i32 -> 2
                                                 let v' = (v.f32, v.i32)
                                                 in (t', v')) args)
                    let (fs, is) = unzip vs
                    in (s, ts, fs, is, colour)) inps)

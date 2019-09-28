# Rendering of the Julia Set ([Accelerate version](https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/julia))

Mostly a straightforward port, and most of the controls are the same.

| Key | Action |
| --- | ------ |
| Left mouse button | Drag the view port |
| q/e | Change the speed. |
| w/s | Zoom in and out. |
| z/c | Decrease and increase the radius for determining escape trajectories. |
| a/d | Change iteration limit. |
| Space | Pause time. |
| Left/Right | Step through time (you should pause first). |
| 0-9 | Go to a predefined part of the Julai set. |
| Escape | Exit |

The Accelerate implementation uses an array of functions to pick
between various presets.  This could get very thorny to do in Futhark
(in Accelerate it causes JIT compilation, which is pretty cool), but
fortunately all the presets have the same shape and are
distinguishable by two floating-point constants.

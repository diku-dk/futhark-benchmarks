# N-body simulation

Based on Accelerate's version from
https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/n-body,
along with a [Lys](https://github.com/diku-dk/lys) frontend for an
interactive visualisation.  Just run

    make run

and watch the dots.  Press space to make them move.  Left-click to add
more bodies in a tight ball.  Right-hold to have the mouse cursor act
as a massive gravitational body.  Use the arrow keys to rotate the
particles.  The rendering is pretty wonky, because it does not take
FoV into account.  If you are competent at 3D geometry, please
contribute a fix!  A few other keys do things as well:


| Key | Action |
| --- | ------ |
| m | Toggle between brute force and Barnes-Hut |
| 1/2 | Decrease/increase theta (used for Barnes-Hut) |
| PageUp/PageDown | Zoom in and out |
| o | Reset particles in "orbit" layout |
| s | Reset particles in "spiral" layout |
| r | Reset particles in "uniform cloud" layout |
| Escape | Exit |


## Barnes-Hut

While `nbody.fut` uses a naive *O(n²)*-squared implementation,
`nbody-bh.fut` uses the Barnes-Hut approximation algorithm, which
involves first computing an octree subdividing the space, enabling
more efficient *O(n log(n))* runtime.  The construction of the octree
is not at all trivial in Futhark, but the performance impact is
significant for high *N*.

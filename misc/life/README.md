Game of Life in Futhark
==

Just run `make` to build, then `python life-gui.py`.  Requires
the Futhark compiler, Pygame and PyOpenCL.  And of course, a working
OpenCL setup.

The program is not interactive, so just look at the pretty colours.
When a cell dies, it will slowly fade out rather than turning white
immediately, leading to interesting explosion-like effects.

A number of command line options are supported.  To run with an
alternative ruleset using four different cells, try:

    python life-gui.py --variant quadlife

HotSpot 2D heat equation
========================

Straightforward port of the OpenMP implementation.  Uses explicit
indexing.  The number of iterations to run is also given in the input
data sets - these are currently all 360.

This is a fairly trivial two-dimensional rank-1 stencil, although with
some unusual edge conditions.  Performance in Futhark is OK.

Visualisation with PyGame
=============

Run `make` to compile what needs to be compiled.  Then `make run` to
run with a standard size.  Use the mouse to draw heat sources, scroll
the mouse to change intensity before you draw, then press Space to
run.  You can use Space to toggle between drawing more heat sources
and continuing the simulation.

Most of the parameters (such as ambient temperature and chip
properties) are hardcoded in the Futhark program.

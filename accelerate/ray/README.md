Ray tracer
==========

The scene is structured slightly different than the one in Accelerate,
because we invert the meaning of the Y axis (in this implementation,
positive Y goes *up* relative to the camera).  The computation of
initial rays is also slightly different, in order to support more
flexible camera placement.  The visual impression and computational
load should be almost the same, however.

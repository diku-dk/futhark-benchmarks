# N-body simulation

Based on Accelerate's version from
https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/n-body,
along with a Python frontend that uses Pygame to implement a
interactive visualisation.  Just run

    make run

and watch the dots.  Left-click to add more bodies in a tight ball.
Right-hold to have the mouse cursor act as a massive gravitational
body.  Hold shift and use the arrow keys to rotate the particles.
After rotation, left- and right-clicking becomes quite inaccurate.  If
you are competent at 3D geometry, please contribute a fix!

See the `benchmark` directory for how two benchmark Futhark up against
Accelerate on different input sizes.


## Program structure

The n-body program represents a body by 10 floating-point values:

  + Three floats for the position
  + One float for the mass
  + Three floats for the velocity
  + Three floats for the acceleration

Like the fluid simulation benchmark, it has an outer sequential loop for
stepping through the simulation, and like the fluid simulation benchmark, the
Accelerate benchmarking system does not measure their benchmark running more
than one iteration (although both Accelerate and Futhark use this feature to
test the implementation correctness).

The benchmark is a naive quadratic implementation of the n-body simulation, and
the loop body looks like this:

```
map
    reduce
        map
```

which is transformed into

```
map
    redomap
```

in Futhark.

## Runtime results on gpu02-diku-apl (GTX 780 Ti)

  n=100:
    Accelerate: 1.120ms
    Futhark:    0.214ms
    Speedup:     5.23x

  n=1000:
    Futhark:    0.265ms
    Accelerate: 1.834ms
    Speedup:     6.92x

  n=10000:
    Futhark:    1.414ms
    Accelerate: 8.954ms
    Speedup:    6.33x

  n=100000:
    Futhark:    87.304ms
    Accelerate: 634.3ms
    Speedup:    7.27x

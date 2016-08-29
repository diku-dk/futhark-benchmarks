# N-body simulation

Based on Accelerate's version from
https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/n-body,
along with a Python frontend that uses Pygame to implement a
interactive visualisation.  Just run

    make run

and watch the dots.  Left-click to add more bodies in a tight ball.
Right-hold to have the mouse cursor act as a massive gravitational
body.

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


## Results

**Run on napoleon.**

### Raw Futhark results

```
dataset nbody-n_steps=1-n_bodies=100-timestep=1.0-epsilon=50.0.in: 460.10us (average; relative standard deviation: 0.10)
dataset nbody-n_steps=1-n_bodies=1000-timestep=1.0-epsilon=50.0.in: 710.60us (average; relative standard deviation: 0.04)
dataset nbody-n_steps=1-n_bodies=10000-timestep=1.0-epsilon=50.0.in: 5311.40us (average; relative standard deviation: 0.01)
dataset nbody-n_steps=1-n_bodies=100000-timestep=1.0-epsilon=50.0.in: 447308.30us (average; relative standard deviation: 0.02)
```


### Raw Accelerate results

```
Accelerate with n_bodies=100, time_step=1.0, and epsilon=50.0.
time                 2.156 ms   (2.055 ms .. 2.257 ms)
                     0.989 R²   (0.985 R² .. 0.994 R²)
mean                 2.159 ms   (2.124 ms .. 2.206 ms)
std dev              141.7 μs   (107.0 μs .. 216.1 μs)
variance introduced by outliers: 48% (moderately inflated)

Accelerate with n_bodies=1000, time_step=1.0, and epsilon=50.0.
time                 2.909 ms   (2.802 ms .. 2.997 ms)
                     0.992 R²   (0.989 R² .. 0.995 R²)
mean                 2.851 ms   (2.797 ms .. 2.906 ms)
std dev              182.2 μs   (159.0 μs .. 217.7 μs)
variance introduced by outliers: 44% (moderately inflated)

Accelerate with n_bodies=10000, time_step=1.0, and epsilon=50.0.
time                 19.92 ms   (19.74 ms .. 20.11 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 20.70 ms   (20.35 ms .. 21.28 ms)
std dev              1.061 ms   (377.6 μs .. 1.600 ms)
variance introduced by outliers: 18% (moderately inflated)

Accelerate with n_bodies=100000, time_step=1.0, and epsilon=50.0.
time                 1.457 s    (1.433 s .. 1.473 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.445 s    (1.437 s .. 1.450 s)
std dev              7.649 ms   (0.0 s .. 8.823 ms)
variance introduced by outliers: 19% (moderately inflated)
```

### Conclusion

Average runtimes:

| Size   | Futhark | Accelerate |
| ------ | ------- | ---------- |
| 100    | 460.10us | 2156.00us |
| 1000   | 710.60us | 2909.00us |
| 10000  | 5311.40us | 19920.00us |
| 100000 | 447308.30us | 1457000.00us |

## Runtime results on gpu01-diku-apl (GTX 780 Ti)

  n=100:
    Accelerate: 0.455ms
    Futhark:    0.029ms
    Speedup:     15.9x

  n=1000:
    Futhark:    0.671ms
    Accelerate: 1.115ms
    Speedup:     1.551x

  n=10000:
    Futhark:    5.407ms
    Accelerate: 8.238ms
    Speedup:    1.5x

  n=100000:
    Futhark:    253.6ms
    Accelerate: 622.3ms
    Speedup:    2.45x

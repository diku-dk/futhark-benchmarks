# Quasicrystals

Based on Accelerate's version at
https://github.com/AccelerateHS/accelerate-examples/tree/master/examples/crystal

Do

    make run

for a nice interactive visualisation.  Use the arrow keys to modify
the parameters of the visualisation.

## Program structure

The crystal benchmark has a fairly simple structure:

```
map
    reduce
        map
```

In Futhark this is turned into

```
map
    redomap
```

The Accelerate benchmark does not recognize the reduce and instead implements it
as a recursive function.


## Runtime results on gpu01-diku-apl (GTX 780 Ti)

    (Accelerate default)
    size=200, scale=30.0, degree=5, n_steps=1, time_delta=1.0:
      Accelerate: 586.3us
      Futhark:    160.0us
      Futhark speedup: 3.66x

    size=2000, scale=30.0, degree=50, n_steps=1, time_delta=1.0:
      Accelerate: 37280.0us
      Futhark:    15325.90us
      Futhark speedup: 2.43x

    size=4000, scale=30.0, degree=50, n_steps=1, time_delta=1.0:
      Accelerate: 126700.0us
      Futhark:     60564.4us
      Futhark speedup: 2.0x


## Visualization

Run `./crystal-make-images.py` to make some pretty pictures.

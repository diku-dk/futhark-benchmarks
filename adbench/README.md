# ADBench

This directory contains benchmarks ported from
[ADBench](https://github.com/microsoft/ADBench). They have been
extracted from the ADBench infrastructure itself. A version that is
properly integrated can be found
[here](https://github.com/diku-dk/ADBench/tree/futhark), although note
that it is no longer maintained, so the Futhark programs may not work.
You should be able to simply overwrite those with the ones that are
here.

The original work (including data conversion) is in [another
repository](https://github.com/diku-dk/futhark-ad).

## Beware

Some of these datasets require a lot of memory, especially when
computing the Jacobians.

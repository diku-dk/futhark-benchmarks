futhark-benchmarks
==================

Futhark implementations of various benchmarks.  The intention is that
these implementations are of existing published benchmarks, and thus
show where we do well or poorly.

You can run everything that runs with:

    futhark-bench .

Although you may want to pass `--compiler=futhark-opencl` to use the
GPU.  Some of the data sets take a very long time with the default
`futhark-c` compiler.

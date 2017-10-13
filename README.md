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

Many of the input/output files are stored in a simple binary format
for performance and space reasons.  This format is [documented in the
Futhark reference manual][0], and can be converted to a textual
representation via the [futhark-dataset][1] tool.

[0]: http://futhark.readthedocs.io/en/latest/binary-data-format.html
[1]: http://futhark.readthedocs.io/en/latest/man/futhark-dataset.html

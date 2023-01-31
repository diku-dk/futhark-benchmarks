# Ports of PBBS

The [Problem Based Benchmark
Suite](https://github.com/cmuparlay/pbbsbench) is a collection of
benchmarks, typically involving significant amounts of irregularity.
The upstream implementations are all in multi-threaded C++, but they
are of high quality, including careful descriptions of input and
output data.

Upstream PBBS doesn't really standardise the workloads for
benchmarking, but instead provides programs for generating input data
of a given size in a variety of bespoke data formats.  For
reproducibility, we prefer pinning the exact data used.  We still use
the PBBS data generates to produce the workloads in the first place,
using the programs [fut2pbbs.c](fut2pbbs) and [pbbs2fut.c](pbbs2fut)
to translate between PBBS and Futhark data formats.

By happy coincidence, the timing method used by `futhark bench`
closely matches that expected by PBBS: timing starts after the input
is in memory, and before it is written to disk.  IO and low-level
initialisation is not counted.  Note however that for the GPU
backends, the input will be resident in *GPU memory*, meaning that
transfer time to and from GPU is also not counted (except transfers
that occur during the benchmark itself).

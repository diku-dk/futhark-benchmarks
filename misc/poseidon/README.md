# Poseidon hash function

This benchmark is based on the
[neptune-triton](https://github.com/filecoin-project/neptune-triton)
implementation of the Poseidon hash function.  The intent is that the
`poseidon.fut` here matches the upstream exactly, and
`poseidon-bench.fut` then contains actual benchmarks.  This means that
the `poseidon.fut` here should not be modified manually, except when
necessary due to language changes, but the upstream version should
then also be modified accordingly.

We only test a few of the entry points defined by `poseidon.fut`.
This makes this a less comprehensive test than might be desirable (the
sheer size of `poseidon.fut` has historically been a bountiful source
of bugs), but at least the performance characteristics should be the
same.

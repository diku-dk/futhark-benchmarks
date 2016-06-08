#!/bin/sh
#
# Measure Futhark's BFS implementation on the large datasets.

cd "$(dirname "$0")/.."
for x in *_more.fut; do
    echo "# $x"
    futhark-bench --compiler=futhark-opencl $x
    echo
    echo
done

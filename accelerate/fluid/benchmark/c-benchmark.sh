#!/bin/sh
#
# Benchmark the C version on the same input sizes as Futhark's fluid-benchmark.

set -e # Exit on first error.

cd "$(dirname "$0")"

run_c_benchmark() {
    n_steps="$1"
    grid_resolution="$2"

    echo "Running benchmark with n_steps=$n_steps and grid_resolution=$grid_resolution."

    ../fluid-generate-random-input.py "$n_steps" "$grid_resolution" c none \
        | ../src-c/fluid-benchmark 1>/dev/null
}

run_c_benchmark 40 100
run_c_benchmark 40 1000

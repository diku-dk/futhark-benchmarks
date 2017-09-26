#!/bin/sh
#
# Benchmark the C version on the same input sizes as Futhark's fluid-benchmark.

set -e # Exit on first error.

cd "$(dirname "$0")"

run_c_benchmark() {
    n_steps="$1"
    n_solver_steps="$2"
    grid_resolution="$3"

    echo "Running benchmark with n_steps=$n_steps, n_solver_steps=$n_solver_steps, and grid_resolution=$grid_resolution."

    ../fluid-generate-random-input.py "$n_steps" "$grid_resolution" "$n_solver_steps" c none \
        | ../src-c/fluid-benchmark 1>/dev/null
}

run_c_benchmark 1 40 100
run_c_benchmark 1 40 1000

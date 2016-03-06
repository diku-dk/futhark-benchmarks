#!/bin/sh
#
# Benchmark accelerate-fluid on the same input sizes as Futhark's
# fluid-benchmark.

set -e # Exit on first error.

run_accelerate_benchmark() {
    n_steps="$1"
    grid_resolution="$2"

    echo "Running benchmark with n_steps=$n_steps and grid_resolution=$grid_resolution."
    
    accelerate-fluid --benchmark=True \
        --viscosity=0.00001 --diffusion=0.0001 --delta=0.1 \
        --iterations="$n_steps" \
        --width="$grid_resolution" --height="$grid_resolution"
}

run_accelerate_benchmark 40 100
run_accelerate_benchmark 40 1000
run_accelerate_benchmark 20 2000
run_accelerate_benchmark 20 3000

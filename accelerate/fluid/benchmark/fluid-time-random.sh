#!/bin/sh

set -e # Exit on first error.

cd "$(dirname "$0")"

n_steps="$1"
grid_resolution="$2"
backend="$3"
seed="$4"

if ! [ "$n_steps" ]; then
    echo 'error: the number of steps must be given as the first argument' > /dev/stderr
    exit 1
fi

if ! [ "$grid_resolution" ]; then
    echo 'error: the grid resolution must be given as the second argument' > /dev/stderr
    exit 1
fi

if ! [ "$backend" ]; then
    echo 'error: backend ("futhark", "c" or "accelerate") must be given as the third argument' > /dev/stderr
    exit 1
fi

if ! [ "$seed" ]; then
    echo 'error: seed (number or "none") must be given as the fourth argument' > /dev/stderr
    exit 1
fi

if [ "$backend" = 'futhark' ] || [ "$backend" = 'c' ]; then
    if ! [ -e "../src-$backend/fluid-benchmark" ]; then
        echo 'error: there is no fluid-benchmark program' > /dev/stderr
        exit 1
    fi
    ../fluid-generate-random-input.py "$n_steps" "$grid_resolution" "$backend" "$seed" \
        | time ../src-"$backend"/fluid-benchmark > /dev/null
elif [ "$backend" = 'accelerate' ]; then
    time accelerate-fluid --benchmark=True \
         --viscosity=0.00001 --diffusion=0.0001 --delta=0.1 \
         --iterations="$n_steps" \
         --width="$grid_resolution" --height="$grid_resolution" \
         --only-run=1

fi

exit 0

#!/bin/sh

set -e # Exit on first error.

cd "$(dirname "$0")"

n_steps="$1"
grid_resolution="$2"
n_solver_steps="$3"
backend="$4"
out_dir="$5"
seed="$6"

if ! [ "$n_steps" ]; then
    echo 'error: the number of steps must be given as the first argument' > /dev/stderr
    exit 1
fi

if ! [ "$grid_resolution" ]; then
    echo 'error: the grid resolution must be given as the second argument' > /dev/stderr
    exit 1
fi

if ! [ "$n_solver_steps" ]; then
    echo 'error: the number of lin_solve steps must be given as the third argument' > /dev/stderr
    exit 1
fi

if ! [ "$backend" ]; then
    echo 'error: backend ("futhark" or "c") must be given as the fourth argument' > /dev/stderr
    exit 1
fi

if ! [ "$out_dir" ]; then
    echo 'error: output directory must be given as the fifth argument' > /dev/stderr
    exit 1
fi

if ! [ "$seed" ]; then
    echo 'error: seed (number or "none") must be given as the sixth argument' > /dev/stderr
    exit 1
fi

if [ -d "$out_dir" ]; then
    echo 'error: output directory already exists' > /dev/stderr
    exit 1
fi

if ! [ -e "../src-$backend/fluid-visualize-densities" ]; then
    echo 'error: there is no fluid-visualize-densities program' > /dev/stderr
    exit 1
fi

../fluid-generate-random-input.py "$n_steps" "$grid_resolution" "$n_solver_steps" "$backend" "$seed" \
    | ../src-"$backend"/fluid-visualize-densities \
    | ./fluid-output-to-images.py "$out_dir" "$backend"

exit 0

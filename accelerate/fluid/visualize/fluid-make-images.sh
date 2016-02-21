#!/bin/sh

set -e # Exit on first error.

cd "$(dirname "$0")"

n_steps="$1"
grid_resolution="$2"
out_dir="$3"

if ! [ "$n_steps" ]; then
    echo 'error: the number of steps must be given as the first argument' > /dev/stderr
    exit 1
fi

if ! [ "$grid_resolution" ]; then
    echo 'error: the grid resolution must be given as the second argument' > /dev/stderr
    exit 1
fi

if ! [ "$out_dir" ]; then
    echo 'error: output directory must be given as the third argument' > /dev/stderr
    exit 1
fi

if [ -d "$out_dir" ]; then
    echo 'error: output directory already exists' > /dev/stderr
    exit 1
fi

if ! [ -e "../src-futhark/fluid-visualize-densities" ]; then
    echo 'error: there is no fluid-visualize-densities program' > /dev/stderr
    exit 1
fi

./fluid-generate-random-input.py "$n_steps" "$grid_resolution" \
    | ../src-futhark/fluid-visualize-densities \
    | ./fluid-output-to-images.py "$out_dir"

exit 0

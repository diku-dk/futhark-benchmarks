#!/bin/sh

set -e # Exit on first error.

cd "$(dirname "$0")"

n_steps="$1"
out_dir="$2"

if ! [ "$n_steps" ]; then
    echo 'error: the number of steps must be given as the first argument' > /dev/stderr
    exit 1
fi

if ! [ "$out_dir" ]; then
    echo 'error: output directory must be given as the second argument' > /dev/stderr
    exit 1
fi

if [ -d "$out_dir" ]; then
    echo 'error: output directory already exists' > /dev/stderr
    exit 1
fi

if ! [ -e "fluid" ]; then
    echo 'error: there is no fluid program' > /dev/stderr
    exit 1
fi

./fluid-generate-input.py "$n_steps" \
    | ./fluid \
    | ./fluid-output-to-images.py "$out_dir"

exit 0

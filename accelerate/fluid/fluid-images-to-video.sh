#!/bin/sh

set -e # Exit on first error.

cd "$(dirname "$0")"

images_dir="$1"

if ! [ "$images_dir" ]; then
    echo 'error: images directory must be given as the first argument' > /dev/stderr
    exit 1
fi

if ! [ -d "$images_dir" ]; then
    echo 'error: images directory does not exist' > /dev/stderr
    exit 1
fi

ffmpeg -hide_banner \
       -i "$images_dir/fluid_%08d.png" -r 25 -n \
       "$(echo "$images_dir" | sed 's/\/$//').webm"

exit 0

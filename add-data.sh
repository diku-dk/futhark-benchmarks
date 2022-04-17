#!/bin/sh
#
# Script to add new files to external-data.txt. Should be run from the root of
# the repository. If the file already exists, we don't add it to
# external-data.txt.

set -e

if [ $# != 2 ]; then
    echo "Usage: $0 URL OUTPUT"
    echo
    echo "For example: $0 https://sid.erda.dk/share_redirect/FlhwY8rtfk/accelerate/canny/lena256.in accelerate/canny/data/lena256.in"
    exit 1
fi

SHA=$(curl "$1" | sha256sum - | cut -f 1 -d ' ')

ln -s -r "external-data/$2" "$2"

echo "external-data/$2 $1 $SHA" >> external-data.txt


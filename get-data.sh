#!/usr/bin/env bash
#
# usage: get-data.sh external-data.txt
#
# external-data.txt file must contain lines of the format:
#
#   PATH URL SHA256SUM
#
# get-data.sh will attempt to download the file at URL into PATH (relative to
# the location of external-data.txt) after verifying that the sha256sum is
# identical to SHA256SUM. Neither field can contain spaces.

set -o errexit
set -o pipefail
set -o nounset

if [ "$#" -ne "1" ]; then
    echo "Usage: $0 FILE"
    echo "FILE must be a file containing lines of the format:"
    echo "   PATH URL SHA256SUM"
    echo "$0 will attempt to download the file at URL into PATH (relative to"
    echo "the location of external-data.txt) after verifying that the sha256sum is"
    echo "identical to SHA256SUM. Neither field can contain spaces."

    exit 3
fi

if [ -z "$(which sha256sum)" ]; then
    echo "Error: sha256sum could not be found."

    exit 4
fi

if [ -z "$(which curl)" ]; then
    echo "Error: curl could not be found."

    exit 5
fi

function longest_line() { cat "$1" | awk '{print length($1)}' | sort -nr | head -1 ; }

BASEDIR=$(dirname "$1")

n=$(longest_line "$1")

while read -r OUTPUT URL CHECKSUM; do
    printf "%-${n}s: " "$OUTPUT"

    if [ -f "$OUTPUT" ]; then
        echo -n "File exists, verifying checksum... "
        COMPUTED_SUM=$(sha256sum "$OUTPUT" | cut -f 1 -d ' ')
        if [ "$COMPUTED_SUM" = "$CHECKSUM" ]; then
            echo "OK."
            continue
        else
            echo "Invalid checksum!"
            echo "Expected $CHECKSUM, got $COMPUTED_SUM."
            echo "Deleting file and redownloading."
            rm "$OUTPUT"
        fi
    fi

    echo "File missing, downloading..."
    TMPFILE=$(mktemp)
    curl --fail "$URL" --output "$TMPFILE"
    COMPUTED_SUM=$(sha256sum "$TMPFILE" | cut -f 1 -d ' ')

    if [ "$COMPUTED_SUM" = "$CHECKSUM" ]; then
        mkdir -p "${BASEDIR}/$(dirname "$OUTPUT")"
        mv "$TMPFILE" "${BASEDIR}/${OUTPUT}"
    else
        echo "Error: Invalid checksum of downloaded file!"
        echo "Expected $CHECKSUM, got $COMPUTED_SUM."
        exit 1
    fi
done < "$1"

echo "Done."

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

if [ "$#" -ne "1" ] && [ "$#" -ne "2" ] || [ "x$2" -ne "x-t" ]; then
    echo "Usage: $0 [-t] FILE"
    echo "FILE must be a file containing lines of the format:"
    echo "   PATH URL SHA256SUM"
    echo "$0 will attempt to download the file at URL into PATH (relative to"
    echo "the location of external-data.txt) after verifying that the sha256sum is"
    echo "identical to SHA256SUM. Neither field can contain spaces."
    echo "If -t is passed, delete the file after downloading it. This is used for testing."

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

DELETE=false

if [ "$#" -eq "1" ]; then
    FILE=$1
elif [ "$#" -eq "2" ]; then
    FILE=$2
    DELETE=true
fi

function longest_line() { cat "$FILE" | awk '{print length($FILE)}' | sort -nr | head -1 ; }

BASEDIR=$(dirname "$FILE")

n=$(longest_line "$FILE")

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
    mkdir -p "${BASEDIR}/$(dirname "$OUTPUT")"
    curl --fail "$URL" --output "${BASEDIR}/${OUTPUT}"
    COMPUTED_SUM=$(sha256sum "${BASEDIR}/${OUTPUT}" | cut -f 1 -d ' ')

    if [ "$COMPUTED_SUM" != "$CHECKSUM" ]; then
        echo "Error: Invalid checksum of downloaded file!"
        echo "Expected: $CHECKSUM"
        echo "Got:      $COMPUTED_SUM"
        exit 1
    fi
    if [ "$DELETE" = "true" ]; then
        rm -f "${BASEDIR}/${OUTPUT}"
    fi
done < "$FILE"

echo "Done."

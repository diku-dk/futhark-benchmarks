#!/bin/sh
#
# Wrapper script around `git-annex addurl` that automatically uses
# --no-check-gitignore and --file.

set -e

if [ $# != 1 ]; then
    echo "Usage: $0 URL"
    exit 1
fi

git-annex addurl --no-check-gitignore "$1" --file "$(basename $1)"

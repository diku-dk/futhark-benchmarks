#!/bin/sh
#
# Use futhark-pkg to upgrade all dependencies for all benchmarks.

for d in $(find * -name futhark.pkg -exec dirname {} \;); do
    echo "Upgrading $d..."
    (cd $d &&
         ( [ "$(futhark pkg upgrade | tee /dev/tty)" = "Nothing to upgrade." ] || futhark pkg sync)
    )
done

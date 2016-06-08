#!/bin/sh

cd "$(dirname "$0")"

./radix_sort_gen.py 10000 10K
./radix_sort_gen.py 100000 100K
./radix_sort_gen.py 1000000 1M

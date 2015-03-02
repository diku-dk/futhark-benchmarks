#! /bin/sh

futhark -s -fe --in-place-lowering -a -e --compile-sequential CalibVolDiff.fut > CalibVolDiff.c
gcc -O3 CalibVolDiff.c -lm
./a.out -t timings.txt < CalibVolDiff.in

#! /bin/sh

futhark -s -fe --in-place-lowering -a -e --compile-sequential LocVolCalib.fut > LocVolCalib.c
gcc -O3 LocVolCalib.c -lm
./a.out -t timings.txt < input.data

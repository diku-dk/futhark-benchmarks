#! /bin/sh

futhark -s -fe --in-place-lowering -a -e --compile-sequential InterestCalib.fut > InterestCalib.c
gcc -O3 InterestCalib.c -lm
./a.out -t timings.txt < input.data

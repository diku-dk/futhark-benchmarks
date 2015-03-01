#! /bin/sh

futhark -s -fe --in-place-lowering -a -e --compile-sequential OptionPricing.fut > OptionPricing.c
gcc -O3 OptionPricing.c -lm
./a.out -t timings.txt < OptionPricing.in

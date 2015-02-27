#! /bin/sh

futhark -s -fe --in-place-lowering -a -e --compile-sequential OptionPricing.fut > OptionPricing.cpp
g++ -O3 -fpermissive OptionPricing.cpp -lm
./a.out -t timings.txt < OptionPricing.in

#!/bin/sh

m=20 #200
n=10 #100
T="32"

futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=1117 -g "[$m][$m][$n]f"$T > x1.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=2117 -g "[$m][$m][$n]f"$T > x2.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=3117 -g "[$m][$m][$n]f"$T > x3.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=4117 -g "[$m][$m][$n]f"$T > x4.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=5117 -g "[$m][$m][$n]f"$T > x5.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=6117 -g "[$m][$m][$n]f"$T > x6.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=7117 -g "[$m][$m][$n]f"$T > y1.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=8117 -g "[$m][$m][$n]f"$T > y2.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=9117 -g "[$m][$m][$n]f"$T > y3.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=3583 -g "[$m][$m][$n]f"$T > y4.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=4583 -g "[$m][$m][$n]f"$T > y5.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=5583 -g "[$m][$m][$n]f"$T > y6.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=6583 -g "[$m]f"$T > a1.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=7583 -g "[$m]f"$T > a2.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=8583 -g "[$m]f"$T > a3.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=9583 -g "[$m]f"$T > a4.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=5743 -g "[$n]f"$T > b1.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=6743 -g "[$n]f"$T > b2.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=7743 -g "[$m]f"$T > c1.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=8743 -g "[$m]f"$T > c2.in
futhark dataset -b --i32-bounds=-100:10000  --seed=9973 -g "[$m][$m]i32" > d1.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=8973 -g "[$m][$m][$n]f"$T > z1.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=7973 -g "[$m][$m][$n]f"$T > z2.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=6973 -g "[$m][$m][$n]f"$T > z3.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=5973 -g "[$m][$m]f"$T > f1.in
cat x1.in x2.in x3.in x4.in x5.in x6.in y1.in y2.in y3.in y4.in y5.in y6.in a1.in a2.in a3.in a4.in b1.in b2.in c1.in c2.in d1.in z1.in z2.in z3.in f1.in > tke32-small.in
rm x1.in x2.in x3.in x4.in x5.in x6.in y1.in y2.in y3.in y4.in y5.in y6.in
rm a1.in a2.in a3.in a4.in b1.in b2.in c1.in c2.in d1.in z1.in z2.in z3.in f1.in

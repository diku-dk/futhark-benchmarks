#!/bin/bash
m=100 #57600
T="32"
arrs="[100][115]f32"
futhark dataset -b "--f"$T"-bounds=0.3:1.3" --seed=1117 -g $arrs > a.in
futhark dataset -b "--f"$T"-bounds=0.4:1.4" --seed=3583 -g $arrs > b.in
futhark dataset -b "--f"$T"-bounds=0.5:1.5" --seed=5743 -g $arrs > c.in
futhark dataset -b "--f"$T"-bounds=0.2:1.2" --seed=9973 -g $arrs > d.in
cat a.in b.in c.in d.in > tridiag32-small.in
rm a.in b.in c.in d.in

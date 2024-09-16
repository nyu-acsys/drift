#!/bin/bash

for f in `ls ../../../drift/tests/effects/ml_tuple/*.ml`; do
  time /tools/home/eric/mochi-clone-mihai/MoCHi/src/mochi.exe $f >> /tmp/newMochi
done

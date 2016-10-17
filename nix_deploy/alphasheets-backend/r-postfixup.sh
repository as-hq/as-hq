#!/usr/bin/env bash

crp=$(patchelf --print-rpath $out/lib/R/lib/libR.so)
patchelf --set-rpath "$out/lib/R/lib:$crp" $out/lib/R/lib/libR.so

#!/usr/bin/env bash
set -ue

BENCH_PATH=~/rodinia/cuda/
NUM_BENCH=22
ALL_BENCHMARKS=(backprop bfs b+tree cfd dwt2d gaussian heartwall hotspot hotspot3D huffman hybridsort kmeans lavaMD leukocyte lud myocyte nn nw particlefilter pathfinder srad_v1 srad_v2 streamcluster)


profile_all () {
    for i in $(seq 0 $NUM_BENCH)
    do
        cd $BENCH_PATH${ALL_BENCHMARKS[$i]}
        echo $BENCH_PATH${ALL_BENCHMARKS[$i]}
        if test -f "metrics.csv"; then
            echo
        else
            #ls [0-9][0-9][0-9]*
            mv [0-9][0-9][0-9]* metrics.csv
        fi
    done
}

profile_all

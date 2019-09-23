#!/usr/bin/env bash
set -ue

BENCH_PATH=~/rodinia/cuda/
#NUM_BENCH=23
NUM_BENCH=7
#ALL_BENCHMARKS=(backprop bfs b+tree cfd dwt2d gaussian heartwall hotspot hotspot3D huffman hybridsort kmeans lavaMD leukocyte lud mummergpu myocyte nn nw particlefilter pathfinder srad_v1 srad_v2 streamcluster)
ALL_BENCHMARKS=(myocyte nn nw particlefilter pathfinder srad_v1 srad_v2 streamcluster)


profile_events_all () {
    for i in $(seq 0 $NUM_BENCH)
    do
        cd $BENCH_PATH${ALL_BENCHMARKS[$i]}
        sudo /usr/local/cuda-10.0/bin/nvprof --profile-child-processes -e all --csv --log-file "%p" ./profile
        #nvprof --profile-child-processes -e all --csv --log-file "%p" ./profile
        echo ${ALL_BENCHMARKS[$i]}
    done
}

profile_metrics_all () {
    for i in $(seq 0 $NUM_BENCH)
    do
        cd $BENCH_PATH${ALL_BENCHMARKS[$i]}
        sudo /usr/local/cuda-10.0/bin/nvprof --profile-child-processes -m all --csv --log-file "%p" ./profile
        #nvprof --profile-child-processes -e all --csv --log-file "%p" ./profile
        echo ${ALL_BENCHMARKS[$i]}
    done
}

profile_metrics_all

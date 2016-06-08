#!/bin/sh
#
# Measure Rodinia's BFS CUDA implementation on the large datasets.  Note that we
# use a slightly improved version that times the program (excluding memory
# transfer).

# Pre-set this path if needed.
if ! [ "$RODINIA_DIR" ]; then
    RODINIA_DIR="$(dirname "$0")"/../../../../rodinia_3.1
fi

inputs="$(cat <<EOF
../data/4096nodes.in.rodinia
../data/65536nodes.in.rodinia
../data/rod1MW_6/graph1MW_6.txt
../data/high_edge_variance_100K.in.rodiniaformat
../data/few_nodes_many_edges_1000n_1000e.in.rodiniaformat
../data/higher_edge_variance_5K.in.rodiniaformat
EOF
)"

for x in $inputs; do
    echo "# $x"

    "$RODINIA_DIR"/cuda/bfs/bfs $x
    
    echo
    echo
done

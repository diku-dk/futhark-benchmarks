#!/usr/bin/env python3
#
# Generate special datasets for the BFS to test certain cases.  Output in
# Rodinia's format, so that we can also test on their programs.
#
# Very much inspired by Rodinia's graphgen utility.  However, Rodinia might
# generate duplicate edges, especially if the MAX_INIT_EDGES is set to much
# higher than default, and this script will not.

import random


def gen_dataset(n_nodes, n_edges_min, n_edges_max, name):
    tmp_indices = list(range(n_nodes))
    nodes = [[] for i in range(n_nodes)]
    for i in range(n_nodes):
        node = nodes[i]
        n_edges = random.randint(n_edges_min, n_edges_max)
        for j in range(n_edges):
            node_dest = random.randint(0, n_nodes-1)
            node.append(node_dest)
            nodes[node_dest].append(i)

### COSMIN: shuffling the whole damn [0..n_nodes] takes 
###         WAY TOO LONG
#        random.shuffle(tmp_indices)
#        for j in range(n_edges):
#            node_dest = tmp_indices[j]
#            node.append(node_dest)
#            nodes[node_dest].append(i)

    fname = name + '.in.rodiniaformat'
    with open(fname, 'w') as f:
        print(len(nodes), file=f)
        total_edges = 0
        for i in range(n_nodes):
            num_edges = len(nodes[i])
            print(total_edges, num_edges, file=f)
            total_edges += num_edges

        source = 0
        print(source, file=f)
        print(total_edges, file=f)

        for i in range(n_nodes):
            for j in range(len(nodes[i])):
                dest = nodes[i][j]
                weight = 0  # dummy, required by Rodinia, but unused
                print(dest, weight, file=f)


def gen_all_datasets():
    gen_dataset(6000, 2000, 2000, '6kn_2ke-ct')
    gen_dataset(6000, 10,   3990, '6kn_2ke-var')
    gen_dataset(400000, 30,   30, '400kn_30e-ct')
    gen_dataset(20000,  10,   1190, '20kn_600e-var')

#    gen_dataset(512, 5, 200, '512nodes_high_edge_variance')

if __name__ == '__main__':
    gen_all_datasets()

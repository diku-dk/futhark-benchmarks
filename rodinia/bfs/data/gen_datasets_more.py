#!/usr/bin/env python3
#
# Generate the large ones.

from gen_datasets import *


def gen_all_extra_datasets():
    gen_dataset(100000, 5, 200, 'high_edge_variance_100K')
    gen_dataset(5000, 5, 5000, 'higher_edge_variance_5K')
    gen_dataset(1000, 1000, 1000, 'few_nodes_many_edges_1000n_1000e')

if __name__ == '__main__':
    gen_all_extra_datasets()

-- ==
--
-- tags { disable }
--
-- compiled input @ data/4096nodes.in
-- output @ data/4096nodes.out
--
-- compiled input @ data/65536nodes.in
-- output @ data/65536nodes.out
--
-- compiled input @ data/1Mnodes.in
-- output @ data/1Mnodes.out
--
-- compiled input @ data/high_edge_variance_100K.in
--
-- compiled input @ data/few_nodes_many_edges_1000n_1000e.in
--
-- compiled input @ data/higher_edge_variance_5K.in
--
-- input @ data/graph1MW_6.in
-- output @ data/graph1MW_6.out


import bfs_parallel_segmented_alternate

let f(): i32 = 0 -- hack

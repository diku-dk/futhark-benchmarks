-- A basic, parallel version of BFS.  It's a bit more roundabout that the
-- sequential one.
-- ==
--
-- input @ data/4096nodes.in
-- output @ data/4096nodes.out
-- input @ data/512nodes_high_edge_variance.in
-- output @ data/512nodes_high_edge_variance.out
-- input @ data/graph1MW_6.in
-- output @ data/graph1MW_6.out

import "lib/bfs_main_typical"
import "lib/bfs_lib"

module BFS = BFSLIB({

  let max(a: i32) (b: i32): i32 =
    if a > b then a else b

  let node_work [n][e]
               (tid: i32,
                e_max: i32,
                cost: [n]i32,
                nodes_start_index: [n]i32,
                nodes_n_edges: [n]i32,
                edges_dest: [e]i32,
                graph_visited: [n]bool): ([e_max]i32, [e_max]i32) =
    let start_index = unsafe nodes_start_index[tid]
    let n_edges = unsafe nodes_n_edges[tid]
    let edge_indices = map (+start_index) (iota e_max)
    let node_ids = map (\(i: i32): i32  ->
                        if i < start_index + n_edges
                        then let node_id = unsafe edges_dest[i]
                             in if !(unsafe graph_visited[node_id])
                                then node_id
                                else -1
                                else -1) (
                     edge_indices)
    let costs = replicate e_max (unsafe cost[tid] + 1)
    in (node_ids, costs)

  let step [n][e]
          (cost: *[n]i32,
           nodes_start_index: [n]i32,
           nodes_n_edges: [n]i32,
           edges_dest: [e]i32,
           graph_visited: [n]bool,
           graph_mask: *[n]bool): (*[n]i32, *[n]bool, *[]i32) =
    let active_indices =
      filter (\i -> graph_mask[i]) (iota n)
    let n_indices = (shape active_indices)[0]

    -- We calculate the maximum number of edges for a node.  This is necessary,
    -- since the number of edges are irregular, and since we want to construct a
    -- nested array.
    let e_max = reduce_comm max 0 (nodes_n_edges)

    let changes = map (\(i: i32): ([e_max]i32, [e_max]i32)  ->
                       node_work(i, e_max, cost, nodes_start_index,
                                 nodes_n_edges, edges_dest, graph_visited)
                      ) (active_indices)

    let (changes_node_ids, changes_costs) =
      unzip(changes)

    let full_length = e_max * n_indices
    let node_ids = reshape (full_length) changes_node_ids
    let costs = reshape (full_length) changes_costs

    let cost' = scatter cost node_ids costs

    let graph_mask' =
      scatter graph_mask active_indices (replicate n_indices false)

    in (cost', graph_mask', node_ids)
})

let main [n][e] (nodes_start_index: [n]i32, nodes_n_edges: [n]i32, edges_dest: [e]i32): [n]i32 =
  BFS.common_main(nodes_start_index, nodes_n_edges, edges_dest)

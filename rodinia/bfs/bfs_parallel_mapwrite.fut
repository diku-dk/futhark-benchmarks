-- A basic, parallel version of BFS.  It's a bit more roundabout that the
-- sequential one.
-- ==
--
-- tags { }
-- input @ data/4096nodes.in
-- output @ data/4096nodes.out
-- input @ data/512nodes_high_edge_variance.in
-- output @ data/512nodes_high_edge_variance.out
-- input @ data/graph1MW_6.in
-- output @ data/graph1MW_6.out

import "lib/bfs_main_typical"

module BFS = BFSLIB({

  let max(a: i32) (b: i32): i32 =
    if a > b then a else b

  let step [n][e] (cost: *[n]i32,
                   nodes_start_index: [n]i32,
                   nodes_n_edges: [n]i32,
                   edges_dest: [e]i32,
                   graph_visited: [n]bool,
                   graph_mask: *[n]bool): (*[n]i32, *[n]bool, *[]i32) =

    -- We calculate the maximum number of edges for a node.  This is necessary,
    -- since the number of edges are irregular, and since we want to construct a
    -- nested array.
    let e_max = i32.maximum nodes_n_edges

    let (inds_mask, ind_vals_upd0) =
      unzip(map (\(tid: i32): (i32, [e_max](i32,i32))  ->
                 let start_index = nodes_start_index[tid]
                 let n_edges     = nodes_n_edges[tid]
                 let new_cost    = cost[tid] + 1

                 let mask        = graph_mask[tid]
                 let ind_mask    = if mask then tid else -1
                 let ind_val_upd =
                   map (\(k: i32): i32  ->
                        let i  = start_index + (if k < n_edges
                                                then k
                                                else (n_edges - 1))
                        let id = unsafe edges_dest[i]
                        let already_visited = unsafe graph_visited[id]
                        in if mask && (!already_visited) then id else -1)
                 (iota e_max)
                 in (ind_mask, zip (ind_val_upd) (replicate e_max new_cost)))
            (iota n))

    let (inds_upd, vals_cost) = unzip(reshape (n*e_max) ind_vals_upd0)
    let vals_mask = replicate n false

    -- Finally, the write phase.
    let graph_mask' = scatter graph_mask inds_mask vals_mask
    let cost'       = scatter cost inds_upd  vals_cost

    in (cost', graph_mask', inds_upd)
})

let main [n][e] (nodes_start_index: [n]i32, nodes_n_edges: [n]i32, edges_dest: [e]i32): [n]i32 =
  BFS.common_main(nodes_start_index, nodes_n_edges, edges_dest)

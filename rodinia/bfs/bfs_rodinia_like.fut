-- A Rodinia-like parallel version of BFS;
-- no filtering, assumes fusion can be applied aggressively.
-- Suppossed to work well when the number of edges per node is small.
-- ==
--
-- tags { }
-- compiled input @ data/4096nodes.in
-- output @ data/4096nodes.out
-- compiled input @ data/512nodes_high_edge_variance.in
-- output @ data/512nodes_high_edge_variance.out
-- compiled input @ data/graph1MW_6.in
-- output @ data/graph1MW_6.out

--import "lib/bfs_lib"

import "/futlib/array"

  let max(a: i32) (b: i32): i32 =
    if a > b then a else b

  let step(cost: *[#n]i32,
           nodes_start_index: [#n]i32,
           nodes_n_edges: [#n]i32,
           edges_dest: [#tot_num_edges]i32,
           graph_visited: [#n]bool,
           graph_mask: *[#n]bool,
           updating_graph_mask : *[#n]bool) : (*[n]i32, *[n]bool, *[n]bool) =
    let e_max_per_node = reduce_comm max 0 nodes_n_edges
    let flat_size = n*e_max_per_node
    let graph_mask_copy = copy graph_mask
    let cost_copy       = copy cost
    
    let bigtuple= map (\ii -> let row = ii / e_max_per_node
                              in 
                              if unsafe (!graph_mask[row])
                              then ((-1,true), (-1,-1))
                              else
                              let col = ii % e_max_per_node
                              let one = if col == 0 then (row,false) else (-1,true)
                              let n_edges = unsafe nodes_n_edges[row]
                              let two = unsafe
                                  if col < n_edges
                                  then -- let start_index = unsafe start_indices[row]
                                       let start_index = unsafe nodes_start_index[row]
                                       let edge_index  = col+start_index
                                       let node_id = unsafe edges_dest[edge_index]
                                       in  if !(unsafe graph_visited[node_id])
                                           -- then (node_id, active_costs[row])
                                           then (node_id, (unsafe cost[row]) + 1)
                                           else (-1, -1)
                                  else (-1, -1)
                              in (one,two)
                      ) (iota flat_size)

    let (changes_mask, changes) = unzip bigtuple
    let (active_indices, mask_vals) = unzip changes_mask
    let (changes_node_ids, changes_costs) = unzip changes

    let updating_graph_mask' =
      scatter updating_graph_mask changes_node_ids (replicate flat_size true)

    let cost' = 
      scatter cost_copy changes_node_ids changes_costs

    let graph_mask' =
      scatter graph_mask_copy active_indices mask_vals

    in (cost', graph_mask', updating_graph_mask')

let common_main(nodes_start_index: [#n]i32,
                  nodes_n_edges: [#n]i32,
                  edges_dest: [#e]i32): [n]i32 =
    let source = 0
    let (graph_mask, graph_visited, cost) = unzip (
        map (\i ->  if i==source 
                    then (true,true,0) 
                    else (false,false,-1) 
            ) (iota n)
      )
    loop ((cost, graph_mask, graph_visited, updating_graph_mask, continue) =
          (cost, graph_mask, graph_visited, replicate n false, true)) =
      while continue do
        let (cost', graph_mask', updating_graph_mask') =
              step( cost,
                    nodes_start_index,
                    nodes_n_edges,
                    edges_dest,
                    graph_visited,
                    graph_mask,
                    updating_graph_mask)

        let step2_inds = map (\i -> if (updating_graph_mask'[i]) then i else (-1)) (iota n)

        let graph_visited' =
            scatter graph_visited step2_inds (replicate n true)

        let graph_mask'' =
            scatter graph_mask' step2_inds (replicate n true)

        let updating_graph_mask'' = 
            scatter (copy updating_graph_mask') step2_inds (replicate n false)

        let continue_indices = map (\x -> if x>=0 then 0 else -1) step2_inds
        let continue' = 
            scatter (copy [false]) continue_indices (replicate n true)

        in (cost', graph_mask'', graph_visited', updating_graph_mask'', (continue'[0]))
    in cost

let main(nodes_start_index: [#n]i32, nodes_n_edges: [#n]i32, edges_dest: [#e]i32): [n]i32 =
  common_main(nodes_start_index, nodes_n_edges, edges_dest)

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

--import "lib/bfs_lib"

import "/futlib/array"

  let max(a: i32) (b: i32): i32 =
    if a > b then a else b

  let step(cost: *[#n]i32,
           nodes_start_index: [#n]i32,
           nodes_n_edges: [#n]i32,
           edges_dest: [#e]i32,
           graph_visited: [#n]bool,
           graph_mask: *[#n]bool,
           updating_graph_mask: *[#n]bool) : (*[n]i32, *[n]bool, *[n]bool) =
    let active_indices =
      filter (\i -> graph_mask[i]) (iota n)
    let n_indices = (shape active_indices)[0]
    let graph_mask' =
      scatter graph_mask active_indices (replicate n_indices false)

    -- We calculate the maximum number of edges for a node.  This is necessary,
    -- since the number of edges are irregular, and since we want to construct a
    -- nested array.
    let e_max = reduce_comm max 0 (nodes_n_edges)

    -- let start_indices = map (\tid -> unsafe nodes_start_index[tid]) active_indices
    -- let act_num_edges = map (\tid -> unsafe nodes_n_edges[tid]    ) active_indices
    -- let active_costs  = map (\tid -> unsafe cost[tid]+1           ) active_indices
    -- let e_max = reduce_comm max 0 act_num_edges

    let flat_len = e_max * n_indices
    let changes = map (\ii -> let row = ii / e_max
                              let col = ii % e_max
                              -- let n_edges     = unsafe act_num_edges[row]
                              let tid     = unsafe active_indices[row]
                              let n_edges = unsafe nodes_n_edges[tid]
                              in  unsafe
                                  if col < n_edges
                                  then -- let start_index = unsafe start_indices[row]
                                       let start_index = unsafe nodes_start_index[tid]
                                       let edge_index  = col+start_index
                                       let node_id = unsafe edges_dest[edge_index]
                                       in  if !(unsafe graph_visited[node_id])
                                           -- then (node_id, active_costs[row])
                                           then (node_id, unsafe cost[tid] + 1)
                                           else (-1, -1)
                                  else (-1, -1)
                      ) (iota flat_len)

    let (changes_node_ids, changes_costs) = unzip(changes)

    let cost' = 
        scatter (copy cost) changes_node_ids changes_costs
    
    let updating_graph_mask' = 
        scatter updating_graph_mask changes_node_ids (replicate flat_len true)

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
    let (cost,_,_,_,_) =
      loop ((cost, graph_mask, graph_visited, updating_graph_mask, continue) =
            (cost, graph_mask, graph_visited, replicate n false, true))
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

        in (cost', graph_mask'', graph_visited', updating_graph_mask'', continue'[0])

    in cost

let main(nodes_start_index: [#n]i32, nodes_n_edges: [#n]i32, edges_dest: [#e]i32): [n]i32 =
  common_main(nodes_start_index, nodes_n_edges, edges_dest)

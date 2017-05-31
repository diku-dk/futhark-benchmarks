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
           graph_mask: *[#n]bool): (*[n]i32, *[n]bool, *[]i32) =
    let active_indices =
      filter (\i -> graph_mask[i]) (iota n)
    let n_indices = (shape active_indices)[0]
    let graph_mask' =
      scatter graph_mask active_indices (replicate n_indices false)

    -- We calculate the maximum number of edges for a node.  This is necessary,
    -- since the number of edges are irregular, and since we want to construct a
    -- nested array.
    let e_max = reduce_comm max 0 (nodes_n_edges)

    let start_indices = map (\tid -> unsafe nodes_start_index[tid]) active_indices
    let act_num_edges = map (\tid -> unsafe nodes_n_edges[tid]    ) active_indices
    let active_costs  = map (\tid -> unsafe cost[tid]+1           ) active_indices
    -- let e_max = reduce_comm max 0 act_num_edges


--    let goOn = reduce (||) false
--               (map (\ start_index n_edges ->
--                        let (go,i) = (false,0)
--                        loop ((i,go)) = while (i < n_edges) && (!go) do
--                            let node_id = unsafe edges_dest[i+start_index]
--                            in  (i+1, !(unsafe graph_visited[node_id]))
--                        in go
--                    )
--                    start_indices act_num_edges)

--    let graph_not_visited = 
--        map (\ii -> let row = ii / e_max
--                    let col = ii % e_max
--                    let start_index = unsafe start_indices[row]
--                    let n_edges     = unsafe act_num_edges[row]
--                    let edge_index  = col+start_index
--                    in  unsafe
--                        if col < n_edges
--                        then let node_id = unsafe edges_dest[edge_index]
--                             in  !(unsafe graph_visited[node_id])
--                        else false
--            ) (iota (e_max*n_indices))
--    let goOn = reduce (||) false graph_not_visited

    let changes = map (\ii -> let row = ii / e_max
                              let col = ii % e_max
                              let n_edges     = unsafe act_num_edges[row]
                              in  unsafe
                                  if col < n_edges
                                  then let start_index = unsafe start_indices[row]
                                       let edge_index  = col+start_index
                                       let node_id = unsafe edges_dest[edge_index]
                                       in  if !(unsafe graph_visited[node_id])
                                           then (node_id, active_costs[row])
                                           else (-1, -1)
                                  else (-1, -1)
                      ) (iota (e_max*n_indices))

    let (changes_node_ids, changes_costs) = unzip(changes)

    let cost' = scatter cost changes_node_ids changes_costs

    in (cost', graph_mask', changes_node_ids)

let common_main(nodes_start_index: [#n]i32,
                  nodes_n_edges: [#n]i32,
                  edges_dest: [#e]i32): [n]i32 =
    let graph_mask = replicate n false
    let graph_visited = replicate n false
    let source = 0
    let graph_mask[source] = true
    let graph_visited[source] = true
    let cost = replicate n (-1)
    let cost[source] = 0
    loop ((cost, graph_mask, graph_visited, continue) =
          (cost, graph_mask, graph_visited, true)) =
      while continue do
        let (cost', graph_mask', updating_indices) =
              step( cost,
                    nodes_start_index,
                    nodes_n_edges,
                    edges_dest,
                    graph_visited,
                    graph_mask)

        let n_indices = (shape updating_indices)[0]

        let graph_mask'' =
            scatter graph_mask' updating_indices (replicate n_indices true)

        let graph_visited' =
            scatter (copy graph_visited) updating_indices (replicate n_indices true)

        let continue_indices = map (\x -> if x>=0 then 0 else -1) updating_indices
        let continue' = 
            scatter (copy [false]) continue_indices (replicate n_indices true)

        in (cost', graph_mask'', graph_visited', continue'[0])
    in cost

let main(nodes_start_index: [#n]i32, nodes_n_edges: [#n]i32, edges_dest: [#e]i32): [n]i32 =
  common_main(nodes_start_index, nodes_n_edges, edges_dest)

-- A naive, sequential version of BFS.  Its purpose is to exist as a simple,
-- working solution.
-- ==
-- tags { nobench }
--
-- compiled input @ data/4096nodes.in
-- output @ data/4096nodes.out
-- compiled input @ data/512nodes_high_edge_variance.in
-- output @ data/512nodes_high_edge_variance.out
-- input @ data/graph1MW_6.in
-- output @ data/graph1MW_6.out
-- input @ data/64kn_8e-var-1-128-skew.in
-- output @ data/64kn_8e-var-1-128-skew.out

import "/futlib/array"

let node_work [n][e] (tid: i32,
                      cost: *[n]i32,
                      nodes_start_index: [n]i32,
                      nodes_n_edges: [n]i32,
                      edges_dest: [e]i32,
                      graph_visited: [n]bool,
                      graph_mask: *[n]bool,
                      updating_graph_mask: *[n]bool): (*[n]i32, *[n]bool, *[n]bool) =
  let start_index = nodes_start_index[tid]
  let n_edges = nodes_n_edges[tid]
  let graph_mask[tid] = false
  let (cost, updating_graph_mask) =
    loop ((cost, updating_graph_mask)) for i in steps start_index n_edges 1 do
      let id = edges_dest[i]
      let visited = graph_visited[id]
      in if ! visited
         then
           let cost[id] = cost[tid] + 1
           let updating_graph_mask[id] = true
           in (cost, updating_graph_mask)
         else
           (cost, updating_graph_mask)
    in (cost, graph_mask, updating_graph_mask)

let step [n][e] (cost: *[n]i32,
                 nodes_start_index: [n]i32,
                 nodes_n_edges: [n]i32,
                 edges_dest: [e]i32,
                 graph_visited: [n]bool,
                 graph_mask: *[n]bool,
                 updating_graph_mask: *[n]bool): (*[n]i32, *[n]bool, *[n]bool) =
  let active_indices =
    filter (\(i: i32): bool  -> graph_mask[i]) (iota n)

  -- This loop is a kernel in Rodinia.  Futhark's regularity makes this a bit
  -- tricky to express as a map.
  in loop ((cost, graph_mask, updating_graph_mask))
     for indices_i < length active_indices do
       let i = active_indices[indices_i]
       in node_work(i, cost, nodes_start_index, nodes_n_edges,
                    edges_dest, graph_visited,
                    graph_mask, updating_graph_mask)

let main [n][e] (nodes_start_index: [n]i32,
                 nodes_n_edges: [n]i32,
                 edges_dest: [e]i32): [n]i32 =
  let graph_mask = replicate n false
  let updating_graph_mask = replicate n false
  let graph_visited = replicate n false
  let source = 0
  let graph_mask[source] = true
  let graph_visited[source] = true
  let cost = replicate n (-1)
  let cost[source] = 0
  let (cost, _, _, _, _) =
    loop (cost, updating_graph_mask, graph_mask, graph_visited, continue) =
         (cost, updating_graph_mask, graph_mask, graph_visited, true)
    while continue do
      let (cost', graph_mask', updating_graph_mask') =
        step(cost,
             nodes_start_index,
             nodes_n_edges,
             edges_dest,
             graph_visited,
             graph_mask,
             updating_graph_mask)

      let continue' = false
      let (graph_mask', graph_visited, updating_graph_mask', continue') =
        loop ((graph_mask', graph_visited, updating_graph_mask', continue'))
        for tid < n do
           if updating_graph_mask'[tid]
           then let graph_mask'[tid] = true
                let graph_visited[tid] = true
                let updating_graph_mask'[tid] = false
                in (graph_mask', graph_visited, updating_graph_mask', true)
           else (graph_mask', graph_visited, updating_graph_mask', continue')
      in (cost', updating_graph_mask', graph_mask', graph_visited, continue')
  in cost

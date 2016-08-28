-- A naive, sequential version of BFS.  Its purpose is to exist as a simple,
-- working solution.
-- ==
--
-- compiled input @ data/4096nodes.in
-- output @ data/4096nodes.out
-- compiled input @ data/512nodes_high_edge_variance.in
-- output @ data/512nodes_high_edge_variance.out

fun main(nodes_start_index: [n]i32,
                  nodes_n_edges: [n]i32,
                  edges_dest: [e]i32): [n]i32 =
  let graph_mask = replicate n False
  let updating_graph_mask = replicate n False
  let graph_visited = replicate n False
  let source = 0
  let graph_mask[source] = True
  let graph_visited[source] = True
  let cost = replicate n (-1)
  let cost[source] = 0 in
  loop ((cost, updating_graph_mask, graph_mask, graph_visited, continue) =
        (cost, updating_graph_mask, graph_mask, graph_visited, True)) =
    while continue do
      let (cost', graph_mask', updating_graph_mask') =
        step(cost,
             nodes_start_index,
             nodes_n_edges,
             edges_dest,
             graph_visited,
             graph_mask,
             updating_graph_mask) in

      let continue' = False in
      loop ((graph_mask', graph_visited, updating_graph_mask', continue')) =
        for tid < n do
          if updating_graph_mask'[tid]
          then let graph_mask'[tid] = True
               let graph_visited[tid] = True
               let updating_graph_mask'[tid] = False
               in (graph_mask', graph_visited, updating_graph_mask', True)
          else (graph_mask', graph_visited, updating_graph_mask', continue')

      in (cost', updating_graph_mask', graph_mask', graph_visited, continue')
  in cost

fun step(cost: *[n]i32,
       nodes_start_index: [n]i32,
       nodes_n_edges: [n]i32,
       edges_dest: [e]i32,
       graph_visited: [n]bool,
       graph_mask: *[n]bool,
       updating_graph_mask: *[n]bool): (*[n]i32, *[n]bool, *[n]bool) =
  let active_indices =
    filter (fn (i: i32): bool  => graph_mask[i]) (
           iota(n))

  -- This loop is a kernel in Rodinia.  Futhark's regularity makes this a bit
  -- tricky to express as a map.
  loop ((cost, graph_mask, updating_graph_mask)) =
    for indices_i < (shape active_indices)[0] do
      let i = active_indices[indices_i]
      in node_work(i, cost, nodes_start_index, nodes_n_edges,
                   edges_dest, graph_visited,
                   graph_mask, updating_graph_mask)

  in (cost, graph_mask, updating_graph_mask)

fun node_work(tid: i32,
            cost: *[n]i32,
            nodes_start_index: [n]i32,
            nodes_n_edges: [n]i32,
            edges_dest: [e]i32,
            graph_visited: [n]bool,
            graph_mask: *[n]bool,
            updating_graph_mask: *[n]bool): (*[n]i32, *[n]bool, *[n]bool) =
  let start_index = nodes_start_index[tid]
  let n_edges = nodes_n_edges[tid]
  let graph_mask[tid] = False
  loop ((cost, updating_graph_mask)) =
    for start_index <= i < start_index + n_edges do
      let id = edges_dest[i]
      let visited = graph_visited[id]
      in if ! visited
         then
           let cost[id] = cost[tid] + 1
           let updating_graph_mask[id] = True
           in (cost, updating_graph_mask)
         else
           (cost, updating_graph_mask)
    in (cost, graph_mask, updating_graph_mask)

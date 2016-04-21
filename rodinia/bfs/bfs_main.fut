-- Breadth-first search.
--
-- `edges_dest` and `edges_weight` contain the destination node index and
-- weight, respectively, of each edge.  Each pair from `nodes_start_index` and
-- `nodes_n_edges` describe the edges for a node.  This is true to Rodinia's
-- representation (and it makes sense, since the number of edges per node is
-- irregular).
--
-- Note: Rodinia also has a weight for each edge, but BFS doesn't use it.  It
-- could have been represented as `[i32, e] edges_weight` in Futhark.
--
-- Returns the cost for getting to each node from the source node.
--
-- Noticable naming differences from Rodinia's bfs.cu to Futhark:
--   h_graph_nodes[i].starting -> nodes_start_index
--   h_graph_nodes[i].no_of_edges -> nodes_n_edges
--   h_graph_edges[i] -> edges_dest
-- ==
-- tags { disable }

include lib.bfs_lib

fun [i32, n] main([i32, n] nodes_start_index,
                  [i32, n] nodes_n_edges,
                  [i32, e] edges_dest) =
  let graph_mask = replicate(n, False)
  let updating_graph_mask = replicate(n, False)
  let graph_visited = replicate(n, False)
  let source = 0
  let graph_mask[source] = True
  let graph_visited[source] = True
  let cost = replicate(n, -1)
  let cost[source] = 0 in
  loop ({cost, graph_mask, graph_visited, continue} =
        {cost, graph_mask, graph_visited, True}) =
    while continue do
      let {cost', graph_mask', updating_indices} =
        step(cost,
             nodes_start_index,
             nodes_n_edges,
             edges_dest,
             graph_visited,
             graph_mask)
      
      let n_indices = size(0, updating_indices)

      let graph_mask'' =
        write(updating_indices, replicate(n_indices, True), graph_mask')

      let graph_visited' =
        write(updating_indices, replicate(n_indices, True), graph_visited)

      let tmp_arr = map(fn i32 (i32 ind) => if ind == -1 then 0 else 1, updating_indices)
      let n_indices' = reduce(+, 0, tmp_arr)

      let continue' = n_indices' > 0
      in {cost', graph_mask'', graph_visited', continue'}
  in cost

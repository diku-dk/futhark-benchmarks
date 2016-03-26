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
--
-- ==
--
-- input @ data/4096nodes.in
-- output @ data/4096nodes.out

-- FIXME: Parallelize.
include bfs_step_sequential

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
  loop ({cost, updating_graph_mask, graph_mask, graph_visited, continue} =
        {cost, updating_graph_mask, graph_mask, graph_visited, True}) =
    while continue do
      let {cost', graph_mask', updating_graph_mask'} = 
        step(cost,
             nodes_start_index,
             nodes_n_edges,
             edges_dest,
             graph_visited,
             graph_mask,
             updating_graph_mask)
      let updating_indices =
        filter(fn bool (i32 i) => updating_graph_mask'[i],
               iota(n))
      let n_indices = size(0, updating_indices)

      let graph_mask'' =
        write(updating_indices, replicate(n_indices, True), graph_mask')

      -- FIXME: The current `write` implementation doesn't force uniqueness on
      -- its array argument, so we help it with a hack.  This magic needs to go
      -- away.
      let graph_visited_unique = magic_unique(graph_visited)

      let graph_visited' =
        write(updating_indices, replicate(n_indices, True), graph_visited_unique)

      let updating_graph_mask'' =
        write(updating_indices, replicate(n_indices, False), updating_graph_mask')

      let continue' = size(0, updating_indices) > 0
      in {cost', updating_graph_mask'', graph_mask'', graph_visited', continue'}
  in cost

-- FIXME: See above FIXME.
fun *[bool, n] magic_unique(*[bool, n] foo) = foo

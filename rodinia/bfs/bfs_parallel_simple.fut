-- A basic, parallel version of BFS.  It's a bit more roundabout that the
-- sequential one.
-- ==
--
-- tags { notravis }
-- input @ data/4096nodes.in
-- output @ data/4096nodes.out
-- input @ data/512nodes_high_edge_variance.in
-- output @ data/512nodes_high_edge_variance.out

include lib.bfs_main_typical
include lib.bfs_lib


fun i32 max(i32 a, i32 b) =
  if a > b then a else b

fun (*[n]i32, *[n]bool, *[]i32)
  step(*[n]i32 cost,
       [n]i32 nodes_start_index,
       [n]i32 nodes_n_edges,
       [e]i32 edges_dest,
       [n]bool graph_visited,
       *[n]bool graph_mask) =
  let active_indices =
    i32_filter(graph_mask, iota(n))
  let n_indices = size(0, active_indices)

  -- We calculate the maximum number of edges for a node.  This is necessary,
  -- since the number of edges are irregular, and since we want to construct a
  -- nested array.
  let e_max = reduceComm(max, 0, nodes_n_edges)

  let changes = map(fn ([e_max]i32, [e_max]i32) (i32 i) =>
                      node_work(i, e_max, cost, nodes_start_index,
                                nodes_n_edges, edges_dest, graph_visited)
                   , active_indices)

  let (changes_node_ids, changes_costs) =
    unzip(changes)

  let full_length = e_max * n_indices
  let node_ids = reshape((full_length), changes_node_ids)
  let costs = reshape((full_length), changes_costs)

  let cost' = write(node_ids, costs, cost)

  let graph_mask' =
    write(active_indices, replicate(n_indices, False), graph_mask)

  in (cost', graph_mask', node_ids)

fun ([e_max]i32, [e_max]i32)
  node_work(i32 tid,
            i32 e_max,
            [n]i32 cost,
            [n]i32 nodes_start_index,
            [n]i32 nodes_n_edges,
            [e]i32 edges_dest,
            [n]bool graph_visited) =
  let start_index = unsafe nodes_start_index[tid]
  let n_edges = unsafe nodes_n_edges[tid]
  let edge_indices = map(+ start_index, iota(e_max))
  let node_ids = map(fn i32 (i32 i) =>
                       if i < start_index + n_edges
                       then let node_id = unsafe edges_dest[i]
                            in if ! unsafe graph_visited[node_id]
                               then node_id
                               else -1
                       else -1,
                     edge_indices)
  let costs = replicate(e_max, unsafe cost[tid] + 1)
  in (node_ids, costs)

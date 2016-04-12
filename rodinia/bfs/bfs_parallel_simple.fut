-- A basic, parallel version of BFS.  It's a bit more roundabout that the
-- sequential one.
-- ==
--
-- tags { notravis }
--
-- input @ data/4096nodes.in
-- output @ data/4096nodes.out
-- input @ data/4096nodes_high_edge_variance.in
-- output @ data/4096nodes_high_edge_variance.out

include bfs_main
include lib.bfs_lib

fun i32 max(i32 a, i32 b) =
  if a > b then a else b

fun {*[i32, n], *[bool, n], *[bool, n]}
  step(*[i32, n] cost,
       [i32, n] nodes_start_index,
       [i32, n] nodes_n_edges,
       [i32, e] edges_dest,
       [bool, n] graph_visited,
       *[bool, n] graph_mask,
       *[bool, n] updating_graph_mask) =
  let active_indices =
    i32_filter(graph_mask, iota(n))
  let n_indices = size(0, active_indices)

  -- We calculate the maximum number of edges for a node.  This is necessary,
  -- since the number of edges are irregular, and since we want to construct a
  -- nested array.
  let e_max = reduceComm(max, 0, nodes_n_edges)

  let changes = map(fn {[i32, e_max], [i32, e_max], [bool, e_max]} (i32 i) =>
                      node_work(i, e_max, cost, nodes_start_index,
                                nodes_n_edges, edges_dest, graph_visited,
                                updating_graph_mask),
                    active_indices)

  let {changes_node_ids, changes_costs, changes_updating_graph_masks} =
    unzip(changes)

  let full_length = e_max * n_indices
  let node_ids = reshape((full_length), changes_node_ids)
  let costs = reshape((full_length), changes_costs)
  let updating_graph_masks =
    reshape((full_length), changes_updating_graph_masks)

  let cost' = write(node_ids, costs, cost)

  let graph_mask' =
    write(active_indices, replicate(n_indices, False), graph_mask)

  let updating_graph_mask' =
    write(node_ids, updating_graph_masks, updating_graph_mask)

  in {cost', graph_mask', updating_graph_mask'}

fun {[i32, e_max], [i32, e_max], [bool, e_max]}
  node_work(i32 tid,
            i32 e_max,
            [i32, n] cost,
            [i32, n] nodes_start_index,
            [i32, n] nodes_n_edges,
            [i32, e] edges_dest,
            [bool, n] graph_visited,
            [bool, n] updating_graph_mask) =
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
  let updating_graph_masks = replicate(e_max, True)
  in {node_ids, costs, updating_graph_masks}

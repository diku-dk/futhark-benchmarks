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


fun i32 max(i32 a, i32 b) =
  if a > b then a else b

fun (*[n]i32, *[n]bool, *[]i32)
  step(*[n]i32 cost,
       [n]i32 nodes_start_index,
       [n]i32 nodes_n_edges,
       [e]i32 edges_dest,
       [n]bool graph_visited,
       *[n]bool graph_mask) =

  -- We calculate the maximum number of edges for a node.  This is necessary,
  -- since the number of edges are irregular, and since we want to construct a
  -- nested array.
  let e_max = reduceComm(max, 0, nodes_n_edges)

  let (inds_mask, ind_vals_upd0) =
    unzip(map(fn (i32, [e_max](i32,i32)) (int tid) =>
                let start_index = nodes_start_index[tid]
                let n_edges     = nodes_n_edges[tid]
                let new_cost    = cost[tid] + 1

                let mask        = graph_mask[tid]
                let ind_mask    = if mask then tid else -1
                let ind_val_upd =
                  map(fn i32 (int k) =>
                        let i  = start_index + (if k < n_edges
                                                then k
                                                else (n_edges - 1))
                        let id = unsafe edges_dest[i]
                        let already_visited = unsafe graph_visited[id]
                        in if mask && (!already_visited) then id else -1
                     , iota(e_max))
                in (ind_mask, zip(ind_val_upd, replicate(e_max, new_cost)))
             , iota(n)))

  let (inds_upd, vals_cost) = unzip(reshape((n * e_max), ind_vals_upd0))
  let vals_mask = replicate(n, False)

  -- Finally, the write phase.
  let graph_mask' = write(inds_mask, vals_mask, graph_mask)
  let cost'       = write(inds_upd,  vals_cost, cost)

  in (cost', graph_mask', inds_upd)

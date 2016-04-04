-- A segmented parallel step function for BFS.
-- ==
-- tags { disable }

include lib.bfs_lib

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

  let graph_mask' =
    write(active_indices, replicate(n_indices, False), graph_mask)

  let nodes_start_index' = map(fn i32 (i32 i) => nodes_start_index[i],
                               active_indices)
  let nodes_n_edges' = map(fn i32 (i32 i) => nodes_n_edges[i],
                           active_indices)

  let offsets0 = scan(+, 0, nodes_n_edges')
  let full_length = offsets0[n_indices - 1]
  let offsets = i32_excl_scan_from_incl_scan(offsets0, 0)

  let mask0 = replicate(full_length, False)
  let mask = write(offsets, replicate(n_indices, True), mask0)

  let is0 = replicate(full_length, 1)
  let is1 = write(offsets, nodes_start_index', is0)
  let is2 = i32_plus_scan_segm(is1, mask)

  let node_ids = map(fn i32 (i32 i) => edges_dest[i], is2)
  let write_indices = map(fn i32 (i32 id) =>
                            if graph_visited[id] then -1 else id,
                          node_ids)

  let costs_new0 = replicate(full_length, 0)
  let costs_new1 = write(
    offsets, map(fn i32 (i32 id) => cost[id] + 1, active_indices), costs_new0)
  let costs_new = i32_plus_scan_segm(costs_new1, mask)

  let cost' = write(write_indices, costs_new, cost)
  let updating_graph_mask' =
    write(write_indices, replicate(full_length, True), updating_graph_mask)

  in {cost', graph_mask', updating_graph_mask'}

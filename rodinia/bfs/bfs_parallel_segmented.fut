-- A segmented parallel version of BFS.  It creates helper arrays for each step,
-- contrary to `bfs_parallel_segmented_alternate`.
-- ==
--
-- tags { notravis }
-- input @ data/4096nodes.in
-- output @ data/4096nodes.out
-- input @ data/512nodes_high_edge_variance.in
-- output @ data/512nodes_high_edge_variance.out

include lib.bfs_lib


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
             updating_graph_mask)
      let (updating_indices, n_indices) = get_updating_indices(updating_graph_mask')

      let graph_mask'' =
        write(updating_indices, replicate n_indices True, graph_mask')

      let graph_visited' =
        write(updating_indices, replicate n_indices True, graph_visited)

      let updating_graph_mask'' =
        write(updating_indices, replicate n_indices False, updating_graph_mask')

      let continue' = n_indices > 0
      in (cost', updating_graph_mask'', graph_mask'', graph_visited', continue')
  in cost

fun step(cost: *[n]i32,
       nodes_start_index: [n]i32,
       nodes_n_edges: [n]i32,
       edges_dest: [e]i32,
       graph_visited: [n]bool,
       graph_mask: *[n]bool,
       updating_graph_mask: *[n]bool): (*[n]i32, *[n]bool, *[n]bool) =
  let active_indices =
    i32_filter(graph_mask, iota(n))
  let n_indices = (shape active_indices)[0]

  let graph_mask' =
    write(active_indices, replicate n_indices False, graph_mask)

  let nodes_start_index' = map(fn (i: i32): i32  => unsafe nodes_start_index[i],
                               active_indices)
  let nodes_n_edges' = map(fn (i: i32): i32  => unsafe nodes_n_edges[i],
                           active_indices)

  let offsets0 = scan((+), 0, nodes_n_edges')
  let full_length = offsets0[n_indices - 1]
  let offsets = i32_excl_scan_from_incl_scan(offsets0, 0)

  let mask0 = replicate full_length False
  let mask = write(offsets, replicate n_indices True, mask0)

  let is0 = replicate full_length 1
  let is1 = write(offsets, nodes_start_index', is0)
  let is2 = i32_plus_scan_segm(is1, mask)

  let node_ids = map(fn (i: i32): i32  => unsafe edges_dest[i], is2)
  let write_indices = map(fn (id: i32): i32  =>
                            if unsafe graph_visited[id] then -1 else id,
                          node_ids)

  let costs_new0 = replicate full_length 0
  let costs_new1 =
    write(offsets,
          map(fn (id: i32): i32  => unsafe cost[id] + 1, active_indices),
          costs_new0)
  let costs_new = i32_plus_scan_segm(costs_new1, mask)

  let cost' = write(write_indices, costs_new, cost)
  let updating_graph_mask' =
    write(write_indices, replicate full_length True, updating_graph_mask)

  in (cost', graph_mask', updating_graph_mask')

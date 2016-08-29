-- An alternate segmented parallel version of BFS.  Contrary to
-- `bfs_parallel_segmented`, it creates the helper arrays once and then reuses
-- them.  The downside is that they are always the largest possible sizes, which
-- is not the case in `bfs_parallel_segmented`, where the sizes vary.
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
  let cost[source] = 0

  let offsets0 = scan (+) 0 (nodes_n_edges)
  let offsets = i32_excl_scan_from_incl_scan offsets0 0

  let mask0 = replicate e False
  let mask = write offsets (replicate n True) (mask0)

  let is0 = replicate e 1
  let is1 = write offsets nodes_start_index (is0)
  let is2 = i32_plus_scan_segm(is1, mask)

  let node_ids = map (fn (i: i32): i32  => unsafe edges_dest[i]) is2

  let tids0 = replicate e 0
  let tids1 = write offsets (iota n) (tids0)
  let tids = i32_plus_scan_segm(tids1, mask)

  loop ((cost, graph_mask, graph_visited, continue) =
        (cost, graph_mask, graph_visited, True)) =
    while continue do
      let (cost', graph_mask', updating_indices) =
        step(cost,
             nodes_start_index,
             nodes_n_edges,
             edges_dest,
             graph_visited,
             graph_mask,
             node_ids,
             tids)

      let n_indices = (shape updating_indices)[0]

      let graph_mask'' =
        write updating_indices (replicate n_indices True) (graph_mask')

      let graph_visited' =
        write updating_indices (replicate n_indices True) (graph_visited)

      let tmp_arr = map (fn (ind: int): i32  =>
                          if ind == -1 then 0 else 1) (updating_indices)
      let n_indices' = reduce (+) 0 (tmp_arr)

      let continue' = n_indices' > 0
      in (cost', graph_mask'', graph_visited', continue')
  in cost

fun step(cost: *[n]i32,
       nodes_start_index: [n]i32,
       nodes_n_edges: [n]i32,
       edges_dest: [e]i32,
       graph_visited: [n]bool,
       graph_mask: *[n]bool,
       node_ids: [e]i32,
       tids: [e]i32): (*[n]i32, *[n]bool, *[]i32) =
  let write_indices = map (fn (id: i32, tid: i32): i32  =>
                            if (unsafe graph_visited[id]
                                || ! unsafe graph_mask[tid])
                            then -1
                            else id) (
                          zip (node_ids) tids)

  let costs_new = map (fn (tid: i32): i32  =>
                        unsafe cost[tid] + 1) tids

  let cost' = write write_indices costs_new (cost)

  let masked_indices = map (fn (i: i32): i32  =>
                             if unsafe graph_mask[i] then i else -1) (
                           iota(n))
  let graph_mask' =
    write masked_indices (replicate n False) (graph_mask)

  in (cost', graph_mask', write_indices)

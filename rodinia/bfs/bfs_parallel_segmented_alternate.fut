-- An alternate segmented parallel version of BFS.  Contrary to
-- `bfs_parallel_segmented`, it creates the helper arrays once and then reuses
-- them.  The downside is that they are always the largest possible sizes, which
-- is not the case in `bfs_parallel_segmented`, where the sizes vary.
-- ==
--
-- tags { notravis }
--
-- input @ data/4096nodes.in
-- output @ data/4096nodes.out

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


  let offsets0 = scan(+, 0, nodes_n_edges)
  let offsets = i32_excl_scan_from_incl_scan(offsets0, 0)

  let mask0 = replicate(e, False)
  let mask = write(offsets, replicate(n, True), mask0)

  let is0 = replicate(e, 1)
  let is1 = write(offsets, nodes_start_index, is0)
  let is2 = i32_plus_scan_segm(is1, mask)

  let node_ids = map(fn i32 (i32 i) => edges_dest[i], is2)

  let some_binding = node_ids -- FIXME: This binding should not be necessary,
                              -- but futhark -s spews this out without it:
                              -- "Annotation of "binding of variable lstel_1868"
                              -- type of expression is i32, but derived to be
                              -- bool."

  let tids0 = replicate(e, 0)
  let tids1 = write(offsets, iota(n), tids0)
  let tids = i32_plus_scan_segm(tids1, mask)
  
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
             updating_graph_mask,
             node_ids,
             tids)
      let updating_indices =
        i32_filter(updating_graph_mask', iota(n))
      let n_indices = size(0, updating_indices)

      let graph_mask'' =
        write(updating_indices, replicate(n_indices, True), graph_mask')

      -- FIXME: The current `write` implementation doesn't force uniqueness on
      -- its array argument, so we help it with a hack.  This magic needs to go
      -- away.
      let graph_visited_unique = magic_unique(graph_visited)

      let graph_visited' =
        write(updating_indices, replicate(n_indices, True),
              graph_visited_unique)

      let updating_graph_mask'' =
        write(updating_indices, replicate(n_indices, False),
              updating_graph_mask')

      let continue' = size(0, updating_indices) > 0
      in {cost', updating_graph_mask'', graph_mask'', graph_visited', continue'}
  in cost

-- FIXME: See above FIXME.
fun *[bool, n] magic_unique(*[bool, n] foo) = foo

fun {*[i32, n], *[bool, n], *[bool, n]}
  step(*[i32, n] cost,
       [i32, n] nodes_start_index,
       [i32, n] nodes_n_edges,
       [i32, e] edges_dest,
       [bool, n] graph_visited,
       *[bool, n] graph_mask,
       *[bool, n] updating_graph_mask,
       [i32, e] node_ids,
       [i32, e] tids) =
  let write_indices = map(fn i32 (i32 id, i32 tid) =>
                            if graph_visited[id] || ! graph_mask[tid]
                            then -1
                            else id,
                          zip(node_ids, tids))

  let costs_new = map(fn i32 (i32 tid) => cost[tid] + 1, tids)

  let cost' = write(write_indices, costs_new, cost)
  let updating_graph_mask' =
    write(write_indices, replicate(e, True), updating_graph_mask)

  let graph_mask' =
    write(map(fn i32 (i32 i) => if graph_mask[i] then i else -1, iota(n)),
          replicate(n, False), graph_mask)

  in {cost', graph_mask', updating_graph_mask'}

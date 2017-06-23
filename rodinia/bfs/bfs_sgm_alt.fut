-- An alternate segmented parallel version of BFS.  Contrary to
-- `bfs_parallel_segmented`, it creates the helper arrays once and then reuses
-- them.  The downside is that they are always the largest possible sizes, which
-- is not the case in `bfs_parallel_segmented`, where the sizes vary.
-- ==
--
-- tags { }
-- input @ data/4096nodes.in
-- output @ data/4096nodes.out
-- input @ data/512nodes_high_edge_variance.in
-- output @ data/512nodes_high_edge_variance.out
-- input @ data/graph1MW_6.in
-- output @ data/graph1MW_6.out

import "lib/bfs_lib"
import "/futlib/array"

let step(cost: *[#n]i32,
--       nodes_start_index: [#n]i32,
--       nodes_n_edges: [#n]i32,
--       edges_dest: [#e]i32,
       graph_visited: [#n]bool,
       graph_mask: *[#n]bool,
       node_ids: [#e]i32,
       tids: [#e]i32,
       updating_graph_mask: *[#n]bool): (*[n]i32, *[n]bool) =

  let write_indices = map (\(id: i32, tid: i32): i32  ->
                             if (unsafe graph_visited[id]
                                 || !(unsafe graph_mask[tid]))
                             then -1
                             else id) (zip node_ids tids)

  let costs_new = map (\(tid: i32): i32  ->
                         unsafe cost[tid] + 1) tids

  let cost' = 
        scatter (copy cost) write_indices costs_new
  let updating_graph_mask' = 
        scatter updating_graph_mask write_indices (replicate e true)

  in (cost', updating_graph_mask')


let main(nodes_start_index: [#n]i32,
                  nodes_n_edges: [#n]i32,
                  edges_dest: [#e]i32): [n]i32 =
    let source = 0
    let (graph_mask, graph_visited, cost) = unzip (
        map (\i ->  if i==source 
                    then (true,true,0) 
                    else (false,false,-1) 
            ) (iota n)
      )

  let offsets0 = scan (+) 0 (nodes_n_edges)
  let offsets = i32_excl_scan_from_incl_scan offsets0 0

  let mask0 = replicate e false
  let mask = scatter mask0 offsets (replicate n true)

  let is0 = replicate e 1
  let is1 = scatter is0 offsets nodes_start_index
  let is2 = i32_plus_scan_segm(is1, mask)

  let node_ids = map (\(i: i32): i32  -> unsafe edges_dest[i]) is2

  let tids0 = replicate e 0
  let tids1 = scatter tids0 offsets (iota n)
  let tids = i32_plus_scan_segm(tids1, mask)

  let (cost,_,_,_) =
    loop ((cost, graph_mask, graph_visited, continue) =
          (cost, graph_mask, graph_visited, true))
    while continue do

      let (graph_mask',updating_graph_mask) = unzip (replicate n (false,false))    

      let (cost', updating_graph_mask') =
        step(cost,
--             nodes_start_index,
--             nodes_n_edges,
--             edges_dest,
             graph_visited,
             graph_mask,
             node_ids,
             tids,
             updating_graph_mask)

      let updating_indices = map (\i -> if (updating_graph_mask'[i]) then i else (-1)) (iota n)

      let graph_visited' =
          scatter graph_visited updating_indices (replicate n true)

      let graph_mask'' =
          scatter graph_mask' updating_indices (replicate n true)

      let continue_indices = map (\x -> if x>=0 then 0 else -1) updating_indices
      let continue' = 
          scatter (copy [false]) continue_indices (replicate n true)

      in (cost', graph_mask'', graph_visited', continue'[0])
  in cost

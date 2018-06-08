-- A rather-statical (hoistable all the way) heuristic that attempts to
-- remedy the work complexity by spliting from the very beginning the
-- graph nodes into two sets: one set that has "randomly-distributed"
-- number of edges, and another one that has "abnormally" high number 
-- of edges. The splitting reduces thus the required padding factor. 
-- ==
--
-- tags { }
-- input @ data/4096nodes.in
-- output @ data/4096nodes.out
-- input @ data/512nodes_high_edge_variance.in
-- output @ data/512nodes_high_edge_variance.out
-- input @ data/graph1MW_6.in
-- output @ data/graph1MW_6.out
-- input @ data/64kn_32e-var-1-256-skew.in
-- output @ data/64kn_32e-var-1-256-skew.out

-- Paper's tests!
-- input @ data/bin-6kn_2ke-ct.in
-- output @ data/bin-6kn_2ke-ct.out
-- input @ data/bin-6kn_2ke-var.in
-- output @ data/bin-6kn_2ke-var.out
-- input @ data/bin-400kn_30e-ct.in
-- output @ data/bin-400kn_30e-ct.out
-- input @ data/bin-20kn_600e-var.in
-- output @ data/bin-20kn_600e-var.out
-- input @ data/graph1MW_6.in
-- output @ data/graph1MW_6.out
-- input @ data/64kn_32e-var-1-256-skew.in
-- output @ data/64kn_32e-var-1-256-skew.out


import "/futlib/array"

  let max(a: i32) (b: i32): i32 =
    if a > b then a else b

  let core_step [n] [e]
          ( cost: *[n]i32,
            nodes_start_index: [n]i32,
            nodes_n_edges: [n]i32,
            edges_dest: [e]i32,
            graph_visited: [n]bool,
            updating_graph_mask: *[n]bool,
            e_max: i32, 
            active_indices: []i32) : (*[n]i32, *[n]bool) =

    let costs_now = map (\tid -> unsafe cost[tid]) active_indices
    let flat_len = e_max * (length active_indices)

    let changes = map (\ii -> 
                          let row = ii / e_max
                          let col = ii % e_max
                          -- let n_edges     = unsafe act_num_edges[row]
                          let tid     = unsafe active_indices[row]
                          let n_edges = unsafe nodes_n_edges[tid]
                          in  unsafe
                              if col < n_edges
                              then -- let start_index = unsafe start_indices[row]
                                  let start_index = unsafe nodes_start_index[tid]
                                  let edge_index  = col+start_index
                                  let node_id = unsafe edges_dest[edge_index]
                                  in  if !(unsafe graph_visited[node_id])
                                      then (node_id, costs_now[row]+1)
                                      -- then (node_id, unsafe cost[tid] + 1)
                                      else (-1, -1)
                              else (-1, -1)
                      ) (iota flat_len)

    let (changes_node_ids, changes_costs) = unzip(changes)
    let cost' = scatter cost changes_node_ids changes_costs
    let updating_graph_mask' = 
                scatter updating_graph_mask changes_node_ids
                        (replicate flat_len true)
    in  (cost', updating_graph_mask')


  let step [n][e]
          (cost: *[n]i32,
           nodes_start_index: [n]i32,
           nodes_n_edges: [n]i32,
           edges_dest: [e]i32,
           graph_visited: [n]bool,
           graph_mask: *[n]bool,
           updating_graph_mask: *[n]bool) : (*[n]i32, *[n]bool, *[n]bool) =

    let active_indices =
      filter (\i -> graph_mask[i]) (iota n)
    
    let graph_mask_res =
      scatter graph_mask active_indices (replicate (length active_indices) false)
    
    -- The whole computation of continue' is hoisted to the outermost level.
    let act_num_edges = map (\tid -> unsafe nodes_n_edges[tid]) (iota n)
    let max_num_edges = i32.maximum act_num_edges
    let tot_num_edges = i32.sum     act_num_edges
    let e_max = 3 * ( tot_num_edges / n + 1)
    let continue' = max_num_edges > e_max

    let (cost_res, updating_graph_mask_res) =
       if continue'
       then let (do_inds_now, do_inds_later) =
                 partition (\tid -> unsafe nodes_n_edges[tid] <= e_max) active_indices
            let (cost', updating_graph_mask') = 
                core_step ( cost, nodes_start_index, nodes_n_edges, edges_dest, 
                            graph_visited, updating_graph_mask, e_max, do_inds_now )
            let (cost'', updating_graph_mask'') = 
                core_step ( cost', nodes_start_index, nodes_n_edges, edges_dest, 
                            graph_visited, updating_graph_mask', max_num_edges, do_inds_later )
            in  (cost'', updating_graph_mask'')

       else let (cost', updating_graph_mask') = 
                core_step ( cost, nodes_start_index, nodes_n_edges, edges_dest, 
                            graph_visited, updating_graph_mask, e_max, active_indices )
            in  (cost', updating_graph_mask')

    in (cost_res, graph_mask_res, updating_graph_mask_res)

let common_main [n][e] (nodes_start_index: [n]i32,
                        nodes_n_edges: [n]i32,
                        edges_dest: [e]i32): [n]i32 =
    let source = 0
    let (graph_mask, graph_visited, cost) = unzip (
        map (\i ->  if i==source 
                    then (true,true,0) 
                    else (false,false,-1) 
            ) (iota n)
      )
    let (cost,_,_,_,_) =
      loop (cost, graph_mask, graph_visited, updating_graph_mask, continue) =
           (cost, graph_mask, graph_visited, replicate n false, true)
      while continue do
        let (cost', graph_mask', updating_graph_mask') =
              step( cost,
                    nodes_start_index,
                    nodes_n_edges,
                    edges_dest,
                    graph_visited,
                    graph_mask,
                    updating_graph_mask)

        let step2_inds = map (\i -> if (updating_graph_mask'[i]) then i else (-1)) (iota n)

        let graph_visited' =
            scatter graph_visited step2_inds (replicate n true)

        let graph_mask'' =
            scatter graph_mask' step2_inds (replicate n true)

        let updating_graph_mask'' = 
            scatter (copy updating_graph_mask') step2_inds (replicate n false)

        let continue_indices = map (\x -> if x>=0 then 0 else -1) step2_inds
        let continue' = 
            scatter (copy [false]) continue_indices (replicate n true)

        in (cost', graph_mask'', graph_visited', updating_graph_mask'', continue'[0])

    in cost

let main [n][e] (nodes_start_index: [n]i32, nodes_n_edges: [n]i32, edges_dest: [e]i32): [n]i32 =
    common_main(nodes_start_index, nodes_n_edges, edges_dest)

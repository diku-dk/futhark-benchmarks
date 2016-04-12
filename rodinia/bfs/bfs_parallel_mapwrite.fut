-- A basic, parallel version of BFS.  It's a bit more roundabout that the
-- sequential one.
-- ==
--
-- tags { notravis }
--
-- input @ data/4096nodes.in
-- output @ data/4096nodes.out

include bfs_main
--include lib.bfs_lib

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

  -- We calculate the maximum number of edges for a node.  This is necessary,
  -- since the number of edges are irregular, and since we want to construct a
  -- nested array.
  let e_max = reduceComm(max, 0, nodes_n_edges)

  let {inds_mask, vals_mask, ind_vals_upd0} = unzip (
    map ( fn {i32, bool, [{i32,i32,bool},e_max]} (int tid) =>
            let start_index = nodes_start_index[tid]
            let n_edges     = nodes_n_edges[tid]
            let new_cost    = cost[tid] + 1

            let mask        = graph_mask[tid]            
            let ind_mask    = if mask then tid else -1 -- ind_val = False
            let ind_val_upd = 
                map ( fn i32 (int k) =>
                        let i = start_index + (if k < n_edges then k else (n_edges-1)) 
                        let id = unsafe edges_dest[i] -- nodes_n_edges[i]
                        let already_visited = unsafe graph_visited[id]
                        in  if mask && (!already_visited) then id else -1
                    , iota(e_max) ) 
                -- else replicate(e_max, {-1,new_cost,True})
            in {ind_mask, False, zip(ind_val_upd, replicate(e_max, new_cost), replicate(e_max, True)) }
        , iota(n) )
    )
  let {inds_upd, vals_cost, vals_upd_mask} = unzip( reshape((n*e_max), ind_vals_upd0) )
  --
  -- Finally the write phase
  let graph_mask'          = write(inds_mask, vals_mask,     graph_mask)
  let cost'                = write(inds_upd,  vals_cost,     cost)
  let updating_graph_mask' = write(inds_upd,  vals_upd_mask, updating_graph_mask)

  in {cost', graph_mask', updating_graph_mask'}

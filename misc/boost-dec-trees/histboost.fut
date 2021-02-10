import "lib/github.com/diku-dk/sorts/radix_sort"
import "lib/github.com/diku-dk/segmented/segmented"
import "helpers/util"
import "helpers/tree"
import "helpers/partition"
import "helpers/objective"

import "hist-utils"
import "bins"

-- data_b: data_points where the value has been mapped to bin_id. b-1 bin is nan_values
-- bin_bounds: split_values for tree
-- labels: True labels which is optimized towards
-- preds: predictions after previous iterations
-- max_depth: max depth of the tree
-- l2, eta, gamma: regulazations params
let train_round [n][d] (data: [n][d]u16) (gis: [n]f32) (his: [n]f32) (num_bins: i64)
                       (max_depth: i64) (l2: f32) (eta: f32) (gamma: f32)
                       : ([](i64, f32, bool, i64), i64) =
  -- create tree to scatter into. 
  let tree = replicate 10000 (0i64,f32.nan, false, -1)
  -- nodes consist of id, #num_elements in node
  let root = [n]
  let (_, res, _, _, _, _, offset) = 
    -- nodes : [l_shp](i64, i64) are the active nodes from previous level
    -- tree  : [max_num_nodes](i64, f32, bool, bool) is the tree computed so far,
    --         updated by scatters
    -- i : i32 is the current level in the tree
    -- data : [active_points_length][d]u16, active points at the current level in the tree
    -- gis, his : [active_points_length]f32 of active points at the current level in the tree
    loop (shp, tree, i, data, gis, his, offset) = (root, tree, 0, data, gis, his, 0)
    while i <= max_depth && !(null shp) do
      let active_points_length = length gis
      let gis  = (gis  :> [active_points_length]f32)
      let his  = (his  :> [active_points_length]f32)
      let data = (data :> [active_points_length][d]u16)
      let l_shp = length shp
      let shp = shp :> [l_shp]i64
      let (new_shp, new_tree, new_data, new_gis, new_his, new_offset) =
        -- if last level of tree- all remaining nodes is converted into leafs
        if i == max_depth then
          let leaf_weights = get_leaf_weight gis his shp l2 eta
          let entries = map (\w ->(0i64, w, false, -1)) leaf_weights --|> trace
          let idxs = indices shp |> map (+offset)
          let final_tree = scatter tree idxs entries
          in
            ([], final_tree, [], [], [], offset+l_shp)
        else
          -- flag_arr for calculating offsets to segmented operations matching number of nodes
          --let flag_arr = mkFlagArray shp 0u16 1u16 active_points_length
          let flag_arr = mkFlagArray shp 0 1 active_points_length
          let (new_hist_gis, new_hist_his) = create_histograms data gis his flag_arr l_shp num_bins
          --let ha = trace new_hist_gis
          let splits = search_splits_segs new_hist_gis new_hist_his l2 gamma
    	  -- splits should be [l_shp](i64, f32, bool, bool)
    	  -- (dim_idx, split_val, missing_dir, terminal_flag)
          let terminal_node_flags = map (.3) splits |> map (!)         
          let idxs = iota l_shp
          let (terminal_shp, active_shp, active_splits, data, gis, his, gis', his',
               term_idxs, act_idxs) =
            if and terminal_node_flags then -- all nodes must be split.
              ([], shp, splits, data, gis, his, [], [], [], idxs)
            else if and (map (.3) splits) then -- all nodes are done.
              (shp, [], [], [], [], [], gis, his, idxs, [])
            else
              -- number idxs array. u16 is current max number of nodes.
              --let seg_idxs = scan (+) 0u16 flag_arr
              let seg_idxs = scan (+) 0 flag_arr
              -- create boolean array to remove dead data
              --let cs = map (\i -> let i = i64.u16 (i-1)  in terminal_node_flags[i]) seg_idxs
              let cs = map (\i -> terminal_node_flags[i-1]) seg_idxs
              -- terminal and active shp, node idxs
              --let (active, terminal) = partition (\x -> !(x.2).3) (zip4 shp nodes splits idxs)
              let (active, terminal) =
                partition (\x -> terminal_node_flags[x.2]) (zip3 shp splits idxs)
              let (active_shp, active_splits, act_idxs) = unzip3 active
              let (terminal_shp, _, term_idxs) = unzip3 terminal
              -- special partition2D which only returns active data points, as terminal is ignored
              let data = partition2D_true data cs
              --let (data, _) = zip data cs |> partition (.1) |> (.0) |> unzip
              let l_act = length data
              -- handle computer warnings
              let data = data :> [l_act][d]u16
              let (act_arrs, fin_arrs) = partition (\x -> x.2) (zip3 gis his cs)
              let (gis, his, _) = unzip3 act_arrs :> ([l_act]f32, [l_act]f32, [l_act]bool)
    	      let (gis', his', _) = unzip3 fin_arrs
              in
              (terminal_shp, active_shp, active_splits,
               data, gis, his, gis', his', term_idxs, act_idxs)
              

          let nodes_to_be_written = replicate l_shp (0i64, f32.nan, false, -1)
          -- get terminal leaf values
    	  let leaf_weights = get_leaf_weight gis' his' terminal_shp l2 eta
          let terminal_entries = map (\w ->(0i64, w, false, -1)) leaf_weights
          let nodes_to_be_written = scatter nodes_to_be_written term_idxs terminal_entries
          -- split values in intermediate tree is bin_id
          let num_nodes_in_level = length nodes_to_be_written
          let new_entries =
            map2 (\x i ->
                   let (dim_id, bin_id) = (x.0, f32.u16 x.1)
                   let value = bin_id + 1.0
                   let child = offset+num_nodes_in_level+i*2
                   in
                         (dim_id, value, x.2, child )
                ) active_splits (indices act_idxs) --:> [num_active](i64, f32, bool, bool)
          let nodes_to_be_written = scatter nodes_to_be_written act_idxs new_entries

          let tree =
            if offset+num_nodes_in_level > length tree then
              scatter (replicate (2*offset) (0, f32.nan, false, -1)) (indices tree) tree
            else
              tree
          let tree_full = scatter tree (map (+offset) idxs) nodes_to_be_written
          -- conditions to split at. +1 as we split on bin_id
          let conds = map (\x -> (x.0, x.1+1, x.2)) active_splits
          -- partition_lifted with scatters! faster than permute
          let nan_bin = u16.i64 num_bins
          let isnan (x: u16) : bool = x == nan_bin
          let (new_data, new_gis, new_his, split_shape) =
            partition_lifted_by_vals conds 0u16 (<) isnan active_shp data gis his

          let new_shp = calc_new_shape active_shp split_shape
          in
            (new_shp, tree_full, new_data, new_gis, new_his, offset+num_nodes_in_level)
      in
        (new_shp, new_tree, i+1, new_data, new_gis, new_his, new_offset)
  in
  (res[:offset], offset)
  




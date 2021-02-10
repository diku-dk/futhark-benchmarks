import "lib/github.com/diku-dk/sorts/radix_sort"
import "lib/github.com/diku-dk/segmented/segmented"
import "helpers/util" -- possible import clash with segmented_reduce?

-- Returns the weight of elements in each node
-- gis: gradients for nodes not splitting at level i
-- his: hessians for nodes not splitting at level i
-- shp: number of elements in each node at level i
-- l2, eta: regulization terms.
-- returns [num_nodes]weights
let get_leaf_weight [n][s] (gis: [n]f32) (his: [n]f32) (shp: [s]i64) (l2: f32) (eta: f32)
                       : [s]f32 =
  let terminal_flag_arr = mkFlagArray shp false true n
  let gissums = segmented_reduce (+) 0f32 terminal_flag_arr gis s
  let hissums = segmented_reduce (+) 0f32 terminal_flag_arr his s
  in
    map2 (\gs hs -> eta*(-gs/(hs+l2))) gissums hissums
    -- + min_weight

-- calculates the gain of a split candidate
-- gl: gradients sum of left
-- hl: hessian sum of left
-- g: gradient sum of node
-- h: hessian sum of node
-- Returns (gain, missing_dir)  
let calc_gain (gl: f32) (hl: f32) (g:f32) (h: f32) (l2: f32) (gamma: f32)
         (missing_gis_sum: f32) (missing_his_sum: f32) (cost_no_split: f32) : (f32, bool) =
  let gr = g-gl-missing_gis_sum
  let hr = h-hl-missing_his_sum
  let left = (gl+missing_gis_sum)**2/(missing_his_sum+hl+l2) + gr**2/(hr+l2)
  let right = gl**2/(hl+l2)+ (gr+missing_gis_sum)**2/(missing_his_sum+hr+l2)
  in
    if left >= right then
      (1/2*(left - cost_no_split) - gamma, true)
    else
      (1/2*(right - cost_no_split) - gamma, false)


-- find the best split within a dimension
-- g_hist: gradient sums of elements in each bin
-- h_hist: hessian sums of elements in each bin
-- g: gradient sum of all elements in node
-- h: hessian sum of all elements in node
-- returns (gain, split_val, missing_dir, node_left, node_right)
-- node_left: gradient sum and hessian sum to left child
-- node_right: gradient sum and hessian sum to right child
-- since we are working in bins, the split_val is u16 bin_id
let find_split_hist [m] (g_hist: [m]f32) (h_hist: [m]f32)
                        (l2: f32) (gamma: f32)
                        : (f32, u16, bool) =
  -- if m == 1? handle
  -- missing value sums
  let na_gis_sum = last g_hist
  let na_his_sum = last h_hist
  let n = m-1
  -- possible split_points
  let gls = scan (+) 0.0 (init g_hist) :> [n]f32
  let hls = scan (+) 0.0 (init h_hist) :> [n]f32
  let g = na_gis_sum + last gls
  let h = na_his_sum + last hls
  let cost_no_split = g**2/(h+l2)
  -- calculate quality of splits
  let gains = map2 (\gl hl -> calc_gain gl hl g h l2 gamma na_gis_sum na_his_sum
                              cost_no_split) gls hls 
  let (gains, flags) = unzip gains
  let (best_split_idx, best_gain) = arg_max gains
  let missing_flag = flags[best_split_idx]
  in (best_gain, u16.i64 best_split_idx, missing_flag)


-- finds the best split within each node(segment) at level i
-- it reduces over dimensions [s][d](gain, dimension, seg_id)
-- by applying arg_max over dimensions.
-- For tie breakers the highest dimension is used.
-- splits: [s][d](gain, dimension, seg_id)
-- returns [s](gain, dimension, seg_id)
let find_best_splits [d][s] (gains: [s][d]f32) : [s](i64, f32) =
  let idxs = iota d
  let max ((i1,d1): (i64,f32)) ((i2,d2): (i64,f32)) =
        if d1 > d2 then (i1,d1)
        else if d2 > d1 then (i2,d2)
        else if i1 > i2 then (i1,d1)
        else (i2,d2)
   in 
  map (\x -> reduce_comm max (-1, f32.lowest) (zip idxs x)) gains
      -- map arg_max gains
  
-- search for splits within each feature of all split candidates for each bin
-- g_hist: [d][s][m]f32 gradient sums for each bin in each node in each dimension
-- h_hist: [d][s][m]f32 hessian sums for each bin in each node in each dimension

-- Returns [s](dim_id, bin_split, missing_direction, is_leaf_flag)
-- returns for each node split_dimension split bin 
-- node_left: gradient sum and hessian sum to left child
-- node_right: gradient sum and hessian sum to right child
let search_splits_segs [d][s][m] (g_hists: [d][s][m]f32) (h_hists: [d][s][m]f32)
                                 (l2: f32) (gamma: f32)
                                 : [s](i64, u16, bool, bool) =
  let best_splits_dim =
    map2 (\seg_g_hist seg_h_hist -> --bin_bound -> -- map over each dim
            map2 (\g_hist h_hist -> -- map over each segment
                    find_split_hist g_hist h_hist --bin_bound g h l2 gamma)
                                    l2 gamma)
                 seg_g_hist seg_h_hist
         ) g_hists h_hists :> [d][s](f32, u16, bool) 
  let (gains, split_vals, missing_dirs) = map unzip3 best_splits_dim |> unzip3

  let best_splits_segments = find_best_splits (transpose gains) -- [s](i64, f32)
  
  in
  -- maps over each segments and creates splits if pos gain
  map (\seg_id ->
         let (dim_id, gain) = best_splits_segments[seg_id]
         in
         if gain > 0.0 then
           let split_val = split_vals[dim_id, seg_id]
           let missing_dir = missing_dirs[dim_id, seg_id]
           in
             (dim_id, split_val, missing_dir, false)
         else
             (0, 0u16, false, true)
      ) (iota s)



-- calculates the gradient and hessian sums of the data
-- data: [n][d]u16 bin_id of each feature value of the data
-- gis: [n]f32 gradients for elements
-- his: [n]f32 hessians for elemetns
-- flag_arr: [n]i64 flag_arr representing segment starts
-- num_segs: number of nodes(segments)
-- num_bins: number of bins
-- returns [d][num_nodes][num_bins] of gradient and hessian sums
let create_histograms [n][d] (data: [n][d]u16) (gis: [n]f32) (his: [n]f32)
                      (flag_arr: [n]i64) (num_segs: i64) (num_bins: i64)
                      : ([d][num_segs][num_bins]f32, [d][num_segs][num_bins]f32) =
  -- flat_offsets for reduce by index
  --let seg_offsets = scan (+) 0 flag_arr |> map (\x -> x-1) |> map (\x -> (i64.u16 x) *num_bins)
  let seg_offsets = scan (+) 0 flag_arr |> map (\x -> x-1) |> map (\x -> x *num_bins)
  -- seg_offsets are multiplied with #num_bins to fit flat representation

  in
   map (\dim_bins ->
          let g_hist_entry =  replicate (num_segs*num_bins) 0.0f32
          let h_hist_entry =  replicate (num_segs*num_bins) 0.0f32
          let idxs = map i64.u16 dim_bins |> map2 (+) seg_offsets
          let g_seg_hist = reduce_by_index g_hist_entry (+) 0.0 idxs gis
          let h_seg_hist = reduce_by_index h_hist_entry (+) 0.0 idxs his
          in  ( unflatten num_segs num_bins g_seg_hist
              , unflatten num_segs num_bins h_seg_hist
              )
       ) (transpose data)  --:> [d]( [num_segs][b]f32, [num_segs][b]f32 )
   |> unzip


let create_histograms_seq [n][d] (data: [n][d]u16) (gis: [n]f32) (his: [n]f32)
                      (flag_arr: [n]i64) (num_segs: i64) (num_bins: i64)
                      : ([d][num_segs][num_bins]f32, [d][num_segs][num_bins]f32) =
  let seg_offsets = scan (+) 0 flag_arr |> map (\x -> x-1) |> map (\x -> x *num_bins)
  let gs = replicate (d*num_segs*num_bins) 0f32
  let hs = replicate (d*num_segs*num_bins) 0f32
  let l = num_segs*num_bins
  let (gs, hs) =
    loop (gs, hs) = (gs, hs) for i < d do
      let dim_bins = data[:, i]
      let idxs = map i64.u16 dim_bins |> map2 (+) seg_offsets  --|> map (+l*i)
      -- let gs1 = reduce_by_index gs (+) 0f32 idxs gis -- global hist way to slow x2!!
      -- let hs1 = reduce_by_index hs (+) 0f32 idxs his
      let g_hist_entry =  replicate (l) 0.0f32
      let h_hist_entry =  replicate (l) 0.0f32
      let g_seg_hist = reduce_by_index g_hist_entry (+) 0.0 idxs gis
      let h_seg_hist = reduce_by_index h_hist_entry (+) 0.0 idxs his
      let offsets = (iota l) |> map (+l*i)
      let gs1 = scatter gs offsets g_seg_hist
      let hs1 = scatter hs offsets h_seg_hist
      in
      (gs1, hs1)
  in
    (unflatten_3d d num_segs num_bins gs, unflatten_3d d num_segs num_bins hs)
    
-- special partition2D only returns true values for 2D array
let partition2D_true [n][d] 't (data: [n][d]t) (conds: [n]bool) : [][d]t =
  
  let true_idxs = map i64.bool conds |> scan (+) 0i64 -- fused all together tho
  let num_true = last true_idxs
  in
  if num_true == 0 then
    []
  else
    let idxs = map2 (\c i -> if c then i-1 else -1i64) conds true_idxs
    let ne = head (head data)
    in
    scatter2D (replicate num_true (replicate d ne)) idxs data
    

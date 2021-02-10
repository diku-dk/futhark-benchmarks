import "../lib/github.com/diku-dk/segmented/segmented"
import "util"

-- change i64 flag_arr to u16 flag_arr unlikely more than 2**16 segs (soft cap)
-- -- -- perserves shape, assumes vals non-empty otherwise pass ne?
-- -- operator: val[i] < limit
-- does apply partition within each segment supplied with shp for a 2D array
-- conds: list of tuples with split dimensions and value for each segment
-- ne: neutral element for scatter
-- op: comparison operator
-- shp: number of elements in each segment
-- vals: values to split
-- Returns: split data and split indicies for each segment.
let partition_lifted [n][l][d] 't (conds: [l](i64, t)) (ne: t) (op: t -> t -> bool) (shp: [l]i64)
                                  (vals: [n][d]t) : ([n][d]t, [l]i64) =
  let flag_arr = mkFlagArray shp 0 1 n
  let bool_flag_arr = map bool.i64 flag_arr
  let seg_offsets_idx = scan (+) 0 flag_arr |> map (\x -> x-1)
  let cs = map2 (\v i -> let (dim, cond_val) = conds[i]
                         in op v[dim] cond_val) vals seg_offsets_idx
  let true_ints = map i64.bool cs
  let false_ints = map (\x -> 1-x) true_ints
  let true_offsets = segmented_scan (+) 0 bool_flag_arr true_ints
  let false_offsets = segmented_scan (+) 0 bool_flag_arr false_ints
  let seg_offsets = scanExc (+) 0 shp
  let num_true_in_segs = segmented_reduce (+) 0 bool_flag_arr true_ints l

  let true_val_offsets =
    map2 (\x i -> x + seg_offsets[i]) true_offsets seg_offsets_idx
  let false_val_offsets =
    map2 (\x i -> x + seg_offsets[i] + num_true_in_segs[i])
         false_offsets seg_offsets_idx
  let idxs =
    map3 (\c iT iF -> if c then iT-1 else iF-1) cs true_val_offsets false_val_offsets
  in
  (scatter2D (replicate n (replicate d ne)) idxs vals, num_true_in_segs)




-- does apply partition within each segment supplied with shp for a 2D array
-- conds: list of tuples with split dimensions and value for each segment
-- ne: neutral element for scatter
-- op: comparison operator
-- shp: number of elements in each segment
-- vals: values to split
-- Returns: index scatter permutation to split data and split indicies for each segment
-- idea behind is this to return index permutation and save 2 replicate (1 2D!!) and use permute
-- should save several memory write operations to memory. coalesed reading?!!
let partition_lifted_idx [n][l][d] 't (conds: [l](i64, t)) (op: t -> t -> bool) (shp: [l]i64)
                                  (vals: [n][d]t) : ([n]i64, [l]i64) =
  let flag_arr = mkFlagArray shp 0u16 1u16 n
  let bool_flag_arr = map bool.u16 flag_arr
  let seg_offsets_idx = scan (+) 0u16 flag_arr |> map (\x -> x - 1u16)
  let cs = map2 (\v i -> let (dim, cond_val) = conds[i64.u16 i]
                         in op v[dim] cond_val) vals seg_offsets_idx
  let true_ints = map i64.bool cs
  let false_ints = map (\x -> 1-x) true_ints
  let true_offsets = segmented_scan (+) 0 bool_flag_arr true_ints
  let false_offsets = segmented_scan (+) 0 bool_flag_arr false_ints
  let seg_offsets = scanExc (+) 0 shp
  let num_true_in_segs = segmented_reduce (+) 0 bool_flag_arr true_ints l
  let true_val_offsets =
   map2 (\x i -> x + seg_offsets[i64.u16 i]) true_offsets seg_offsets_idx
  let false_val_offsets =
   map2 (\x i -> x + seg_offsets[i64.u16 i] + num_true_in_segs[i64.u16 i])
        false_offsets seg_offsets_idx
  let idxs =
    map3 (\c iT iF -> if c then iT-1 else iF-1) cs true_val_offsets false_val_offsets
  in
  (scatter (replicate n 1) idxs (iota n), num_true_in_segs)



-- -- -- perserves shape, assumes vals non-empty otherwise pass ne?
-- -- operator: val[i] < limit
-- does apply partition within each segment supplied with shp for a 2D array
-- conds: list of tuples with split dimensions and value for each segment
-- ne: neutral element for scatter
-- op: comparison operator
-- shp: number of elements in each segment
-- vals: values to split
-- Returns: split data and split indicies for each segment.
let partition_lifted_by_vals [n][l][d] 't (conds: [l](i64, t, bool)) (ne: t)
                                  (op: t -> t -> bool) (isnan: t -> bool)
                                  (shp: [l]i64) (vals: [n][d]t)
                                  (gis: [n]f32) (his: [n]f32)
                                  : ([n][d]t, [n]f32, [n]f32, [l]i64) =
  let flag_arr = mkFlagArray shp 0 1 n
  let seg_offsets_idx = scan (+) 0 flag_arr |> map (\x -> x-1)
  let cs = map2 (\v i -> let (dim, cond_val, flag) = conds[i]
                         in op v[dim] cond_val || (isnan v[dim] && flag)) vals seg_offsets_idx
  let true_ints = map i64.bool cs
  let false_ints = map (\x -> 1-x) true_ints
  let bool_flag_arr = map bool.i64 flag_arr
  let true_offsets = segmented_scan (+) 0 bool_flag_arr true_ints
  let false_offsets = segmented_scan (+) 0 bool_flag_arr false_ints
  let seg_offsets = scanExc (+) 0 shp
  let num_true_in_segs = segmented_reduce (+) 0 bool_flag_arr true_ints l
  let true_val_offsets =  map2 (\x i -> x + seg_offsets[i]) true_offsets seg_offsets_idx
  let false_val_offsets = map2 (\x i -> x + seg_offsets[i] + num_true_in_segs[i])
                               false_offsets seg_offsets_idx
  let idxs =
    map3 (\c iT iF -> if c then iT-1 else iF-1) cs true_val_offsets false_val_offsets
  in
  (scatter2D (replicate n (replicate d ne)) idxs vals,
   scatter (replicate n 0f32) idxs gis,
   scatter (replicate n 0f32) idxs his,
   num_true_in_segs)

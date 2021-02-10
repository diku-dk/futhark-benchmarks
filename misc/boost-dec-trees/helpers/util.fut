import "../lib/github.com/diku-dk/sorts/radix_sort"
import "../lib/github.com/diku-dk/segmented/segmented"
           

-- exclusive scan
let scanExc 't [n] (op: t->t->t) (ne: t) (arr : [n]t) : [n]t =
  scan op ne <| map (\i -> if i>0 then arr[i-1] else ne) (iota n)

-- log2 of x
let log2 x = (loop (y,c) = (x,0i32) while y > 1i32 do (y >> 1, c+1)).1

-- -- permutes array
let permute [n][m] 't (xs: [n]t) (idxs: [m]i64): *[m]t =
  map (\i -> xs[i]) idxs

-- -- permutes 2D array
let permute2D 't [m][d][n] (arr: [m][d]t) (inds: [n]i64) : *[n][d]t =
  map (\ind -> map (\j -> arr[ind,j]) (iota d) ) inds
      
-- operator applied elementwise on tuple of length 2
let tuple_math 't (op: t -> t-> t)(n1: (t,t)) (n2: (t,t)) = (op n1.0 n2.0, op n1.1 n2.1)

-- scatter2D array
let scatter2D [m][d][n] 't (arr2D: *[m][d]t) (inds: [n]i64) (vals2D: [n][d]t): *[m][d]t =
  let flat_length = n*d
  let flat_inds = map (\i -> let (k, r) = (i / d, i % d)
                             in (inds[k]*d + r) ) (iota flat_length)
  let res = scatter (flatten arr2D) flat_inds (flatten vals2D :> [flat_length]t)
  in
  unflatten m d res

  -- can be smarter! should take list of node indices
let getChildren (i: i64): [2]i64 =
  [2*i, 2*i+1]

-- arg_max returns the right most if multiple values
let arg_max [n] (xs: [n]f32): (i64,f32) =
    let max ((i1,d1): (i64,f32)) ((i2,d2): (i64,f32)) =
        if d1 > d2 then (i1,d1)
        else if d2 > d1 then (i2,d2)
        else if i1 > i2 then (i1,d1)
        else (i2,d2)
    in reduce_comm max (i64.lowest,f32.lowest) (zip ((iota n)) xs)


-- let first_true [n] (xs: [n]bool) =
--   let max ((i1, c1): (u16, bool)) ((i2, c2): (u16, bool)) : (u16, bool) =
--     if c1 && c2 then
--       (u16.min i1 i2, c1)
--     else if c1 then
--            (i1, c1)
--     else if c2 then
--            (i2, c2)
--     else
--       (i1, c1)
--   in
--   reduce_comm max (u16.highest, false) (zip (indices xs |> map u16.i64) xs)

    
let first_true [n] (xs: [n]f32) (value: f32) =
  let min ((i1, c1): (u16, f32)) ((i2, c2): (u16, f32)) : (u16, f32) =
    if c1 > value && c2 > value then
      (u16.min i1 i2, f32.min c1 c2)
    else if c1 >= value then
           (i1, c1)
    else if c2 >= value then
           (i2, c2)
    else
      (i1, c1)
  in
  reduce min (u16.highest, f32.lowest) (zip (indices xs |> map u16.i64) xs)

-- creates flag array with shape defined by shp and values val
-- r is to specify returned length to handle compiler warnings.
let mkFlagArray 't [m] (shp: [m]i64) (zero: t)       
                       (flag_val: t) (r: i64) : [r]t =
  let shp_ind = scanExc (+) 0 shp
  let vals = replicate m flag_val
  in
  scatter (replicate r zero) shp_ind vals

-- addded resulting length to handle errors (from lib segmented)
-- applies reduce within segment defined by flag array
let segmented_reduce [n] 't (op: t -> t -> t) (ne: t)
                            (flags: [n]bool) (as: [n]t) (r: i64) : [r]t =
  -- Compute segmented scan.  Then we just have to fish out the end of
  -- each segment.
  let as' = segmented_scan op ne flags as
  -- Find the segment ends.
  let segment_ends = rotate 1 flags
  -- Find the offset for each segment end.
  let segment_end_offsets = segment_ends |> map i64.bool |> scan (+) 0
  -- Make room for the final result.  The specific value we write here
  -- does not matter; they will all be overwritten by the segment
  -- ends.
  let scratch = replicate r ne
  -- Compute where to write each element of as'.  Only segment ends
  -- are written.
  let index i f = if f then i-1 else -1
  in scatter scratch (map2 index segment_end_offsets segment_ends) as'

-- from segmented lib
let replicated_iota [n] (reps:[n]i64) (r: i64) : [r]i64 =
  let s1 = scan (+) 0 reps
  let s2 = map2 (\i x -> if i==0 then 0 else x)
                (iota n) (rotate (-1) s1)
  let tmp = reduce_by_index (replicate r 0) i64.max 0 s2 (iota n)
  let flags = map (>0) tmp
  in segmented_scan (+) 0 flags tmp


-- returns the permutation indexes from boolean array along with split index
-- resembles partition
let get_permute_idxs [n] (conds: [n]bool) : (i64, [n]i64) =
  let true_flags = map i64.bool conds
  let true_idxs = scan (+) 0 true_flags
  let false_flags = map (\t -> 1-t) true_flags
  let i =  last true_idxs
  let false_idxs = scan (+) 0 false_flags |> map (+i)
  let idxs = map3 (\c iT iF -> if c then iT-1 else iF-1) conds true_idxs false_idxs
  in
  (i, idxs)


-- Calculates new shape of nodes based on indicies from partition_lifted
-- s1: old shape
-- s2: split indices from partition_lifted
let calc_new_shape [s] (s1: [s]i64) (s2: [s]i64) : []i64 =
 map2 (\i j -> [j, i-j]) s1 s2 |> flatten
-- let segmented_partition_idxs [s] (conds: [s]bool) (shp: [s]i32)
--                                  : ((i32, i32), []i32, [s]i32) =

--   let (i, xs) = get_permute_idxs cs
--   let (i_segs, shp) = get_permute_idxs conds
--   in
--   ((i, i_segs), xs, shp)

-- let (_, new_tree, data, new_leafs) =
--   loop (active_leafs, new_tree, data, new_leafs) = (active_leafs, tree, data, [])
--   while !(null active_leafs) do
--   let leaf_idx = head active_leafs
--         -- add singleton check?
--         let (_, point_idxs, data_points) = filter (\x -> x.0 == leaf_idx) data |> unzip3
--         in
--         if length point_idxs == 1 then -- cannot split node with one ele :) -- min size?
--         let point_idx = (head point_idxs)
--         let weight = eta*(-gis[point_idx]/(his[point_idx]+l2)) --+ min_weight
--         let new_tree = scatter new_tree [leaf_idx-1] [(0, weight, false, false)]

--         in
--         (tail active_leafs, new_tree, data, new_leafs)
--         else
--         let gis = map (\i -> gis[i]) point_idxs
--         let his = map (\i -> his[i]) point_idxs -- scatter conflict?
--         let pos_splits = map (\i -> search_splits data_points[:,i] gis his
--                                                   l2 gamma) (iota d)
--         let (vals, gains, missing_flags) = unzip3 pos_splits
--         let (split_dim, gain) = arg_max gains
--         let value = vals[split_dim]
--         let missing_flag = missing_flags[split_dim]
--         let node_flag = gain > 0.0 && l < (max_depth-1)
--         let (value, data, new_leafs) =
--           if node_flag then
--               -- let (data_lidxs, data_pidx, data_p) = unzip3 data
--               -- let new_data_lidxs = do_split data_lidxs leaf_idx point_idxs data_points
--               --                               split_dim value missing_flag
--               let ndata = do_split data leaf_idx point_idxs data_points split_dim value missing_flag
--               let new_leafs = new_leafs ++ (getChildren leaf_idx)
--               in
--               -- (value, zip3 new_data_lidxs data_pidx data_p, new_leafs)
--               (value, ndata, new_leafs)
--           else
--           let weight = get_leaf_weight gis his l2 eta
--           in
--               --(weight, data, if null new_leafs then new_leafs else tail new_leafs) -- check if lastleaf?
--               (weight, data, new_leafs)
--         let new_tree = scatter new_tree [leaf_idx-1]
--                                [(split_dim, value, node_flag, missing_flag)]
--         in
--         (tail active_leafs, new_tree, data, new_leafs)
-- in
-- (new_leafs, new_tree, data)

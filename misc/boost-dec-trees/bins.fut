import "lib/github.com/diku-dk/sorts/radix_sort"
import "lib/github.com/diku-dk/segmented/segmented"
import "helpers/util"

-- assumes data is sorted
-- given [n] valus then it does equal frequency binning with num_bins-1 as it reserves last bin for
-- na values.
-- if num_unique < bins_left then it simply inserts split values
-- else it searches for split between values
let findBins [n][m] (vals: [n]f32) (num_bins: i64) (dest: *[m]f32): [m]f32 =
  let (na, rest) = partition (f32.isnan) vals
  -- count non-na values and na-values
  let num_values = length rest
  let na_count = length na
  -- flag array with true entries gives an unique segment start
  let unique_start = map2 (!=) rest (rotate (-1) rest) :> [num_values]bool
  -- compiler warning assertion
  let rest = rest :> [num_values]f32

  let distinct_values = zip rest unique_start |> filter (\x -> x.1) |> unzip |> (.0)
  let num_unique = length distinct_values
  let bins_left = num_bins-1
  let bin_vals = 
    if num_unique <= bins_left then
      -- less split_bounds then binds simply map split values
      let bin_vals = map (\i -> if i == (num_unique-1) then
                              distinct_values[i]*2
                            else
                              (distinct_values[i]+distinct_values[i+1])/2.0)
                     (iota num_unique)
      in
        bin_vals
    else
      -- count number of occurences of each unique value
      let counts = segmented_reduce (+) 0 unique_start (replicate num_values 1) num_unique
                                    :> [num_unique]i64
      -- mean number of values in each bin
      let mean_bin_size = f32.i64 (n-na_count) / f32.i64 bins_left
      let sum_counts = scan (+) 0 counts |> map f32.i64
      let tmp = map (\v -> f32.floor( v / mean_bin_size)) sum_counts
      -- cannot split on first unique value and last value is always maximum
      -- else splits are considered between values according to the cumulative counts
      let split_points = map (\i -> if i == 0 then false
                                    else if i == num_unique-1 then true
                                    else tmp[i] != tmp[i-1]) (indices tmp) :> [num_unique]bool

      -- number of splits accumulated, used for calculating distances
      let num_splits = map i64.bool split_points |> scan (+) 0
      let nsplit = last num_splits
      -- returns nan for non splits
      let split_vals =
        map (\i ->
               if i == (num_unique -1) then
                 distinct_values[i]
               else if split_points[i] then
                    let mean_sum = f32.i64 num_splits[i] * mean_bin_size
                    let dist_right = mean_sum - sum_counts[i] |> f32.abs
                    let dist_left = mean_sum -  sum_counts[i-1] |> f32.abs
                    in
                      if dist_left < dist_right then
                        distinct_values[i-1]
                      else
                        distinct_values[i]
               else
                 f32.nan
            ) (indices split_points)
    let active_split_vals = filter (\x -> !(f32.isnan x)) split_vals :> [nsplit]f32
    let split_vals = map (\i -> if i == nsplit-1 then
                                  2* active_split_vals[i] -- last split_val multiplied with 2.
                                                         -- lightgbm does overflow handling? do?
                                   else
                                     (active_split_vals[i] + active_split_vals[i+1])/2.0 
                   ) (indices active_split_vals)
    in
    split_vals
  in
  scatter dest (indices bin_vals) bin_vals with [m-1] = f32.nan
let value_to_bin [n] (value: f32) (bin_bounds: [n]f32) (num_bins: u16) : u16 =
  if f32.isnan value then
     num_bins-1
  else
    -- first_true returns the idxs of first true. since last bin_bounds > max value then
    -- in worst case it returns n-2 index as u16 assumes n <= u16.highest!
    --let (res, _) = map (value<) (init bin_bounds) |> first_true
    --let (res, _) = first_true bin_bounds value
    --in
    --res
      first_true bin_bounds value |> (.0)         
let binMap [n] (vals: [n]f32) (num_bins: i64) : ([n]u16, [num_bins]f32) =
  let dest = replicate num_bins f32.highest
  let s_vals = radix_sort_float f32.num_bits f32.get_bit vals
  let new_bounds = findBins s_vals num_bins dest
  let num_bins = u16.i64 num_bins
  let mapped = map (\v -> value_to_bin v (init new_bounds) num_bins) vals
  in
  (mapped, new_bounds)

  
let binMap_seq [n][d] (data: [d][n]f32) (b: i64)
                      : ([n][d]u16, [d][b]f32) =
  let data_b = replicate d (replicate n 0u16)
  let bounds = replicate d (replicate b 0f32)
  let (data_b, bounds) =
    loop (data_b, bounds) = (data_b, bounds) for i < d do
    let (data_entry, bound_entry) = binMap data[i] b
    in
    (scatter2D data_b [i] [data_entry],
     scatter2D bounds [i] [bound_entry] )
  in
  (transpose data_b, bounds)
  
let binMap_seq_v1 [n][d] (data: [n][d]f32) (b: i64)
                      : ([n][d]u16, [d][b]f32) =
  let data_b = replicate (n*d) 0u16
  let bounds = replicate (b*d) 0f32
  let (data_b, bounds) =
    loop (data_b, bounds) = (data_b, bounds) for i < d do
    let (data_entry, bound_entry) = binMap data[:, i] b
    let offset_b = i*b
    let offset_d = i*n
    let data_offsets = iota n |> map (+offset_d)
    let bound_offsets = iota b |> map (+offset_b)
    in
    (scatter data_b data_offsets data_entry,
     scatter bounds bound_offsets bound_entry)
  in
  (unflatten d n data_b |> transpose, unflatten d b bounds)

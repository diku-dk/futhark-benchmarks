import "../lib/github.com/diku-dk/sorts/radix_sort"
import "util"

let diff [n] (xs: [n]f32) : []f32 =
  map (\i -> xs[i]-xs[i-1]) (iota (n-1) |> map (+1))


let dot [n] (xs: [n]f32) (ys: [n]f32) =
  reduce (+) 0 <| map2 (*) xs ys


let auc_score [n] (y_true: [n]f32) (y_pred: [n]f32) =
  let (s_t, s_p) = zip y_true y_pred
                   |> radix_sort_float_by_key (.1) f32.num_bits f32.get_bit
                   |> reverse |> unzip
  let ha = (scan (+) 0 s_t)
  let unique_idxs = diff s_p |> map (!=0) |> filter (\x -> x) |> map i64.bool |> scanExc (+) 0
  let unique_idxs = unique_idxs ++ [n-1]
  let l = length unique_idxs
  let unique_idxs = unique_idxs :> [l]i64

  --let unique_seg1 = (copy unique_seg) with [length unique_seg-1] = true
  --let (thr, tp, _) = filter (.2) (zip3 s_p ha unique_seg) |> unzip3
  let (_, tps) = permute (zip s_p ha) unique_idxs |> unzip
  let fps  = map2 (\v v1 -> f32.i64 v - v1) (map (+1) unique_idxs ) tps


  let tpr = map (/(last tps)) tps
  let fpr = map (/(last fps)) fps
  let tpr_d = diff tpr ++ [0.0] :> [l]f32
  let fpr_d = diff fpr ++ [0.0] :> [l]f32
  let auc = dot tpr fpr_d + (dot tpr_d fpr_d)/2.0 
  in
  auc



--let main (x: i32) = auc [1, 0, 1, 0, 1] [0.45, 0.4, 0.35, 0.35, 0.8]

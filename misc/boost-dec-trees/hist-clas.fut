import "helpers/util"
import "helpers/objective"
import "helpers/auc"
import "helpers/tree"
import "bins"
import "histboost"

let train_class [n][d] (data: [n][d]f32) (labels: [n]f32) (max_depth: i64) (n_rounds: i64)
                       (l2: f32) (eta: f32) (gamma: f32) = --: [n_rounds]f32 =
  let inital_preds = replicate n 0.5
  let b = 256
  --let (data_b, bin_bounds) = binMap_seq_v1 data b
  let (data_b, bin_bounds) = binMap_seq (transpose data) b

  let trees = replicate 20000 (0i64, f32.nan, false, -1)
  let offsets = replicate n_rounds 0i64

  let (_, trees, offsets, total) =
    loop (preds, trees, offsets, total) =
      (inital_preds, trees, offsets, 0) for i < n_rounds do
      let gis = map2 gradient_log preds labels
      let his = map2 hessian_log preds labels
      let (tree, offset)  = train_round  data_b gis his b max_depth l2 eta gamma 
                               --:> [l](i64, f32, bool, bool) 
      let new_preds = map (\x -> predict_bin x tree b 0) data_b |> map2 (+) preds 
      let mapped_tree =
        map (\x -> let (d, v, miss, flag)= x
                   let (v, flag ) = if flag >= 0 then
                                      (bin_bounds[d, i64.f32 v - 1], flag + total)
                                    else (v, flag)
                   in (d, v, miss, flag)
                            ) tree

      let offsets1 = offsets with [i]=offset
      let trees = if total+offset > length trees then
                    scatter (replicate (2*total) (0i64, f32.nan, false, -1)) (indices trees) trees
                  else
                    trees
      let offsets_tree = map (+total) (indices mapped_tree)
      let new_trees = scatter trees offsets_tree mapped_tree
      in
      (new_preds, new_trees, offsets1, total + offset)
  let flat_ensemble = trees[:total]
  let offsets = scanExc (+) 0 offsets
  let val_error = predict_all data flat_ensemble offsets 0.5
                   |> auc_score labels
  in
  val_error

-- ==
-- entry: main
-- compiled input @ data/small.in.gz
-- output @ data/small.out

entry main [n][d] (data: [n][d]f32) (labels: [n]f32) =
  train_class data labels 6 100 0.5 0.1 0

-- ToDo: check that the code is up to date, i.e., with the new repository
--       at https://github.com/KristianMH/FutharkGBDT

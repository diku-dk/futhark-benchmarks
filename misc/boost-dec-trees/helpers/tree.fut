let rightchild 't (i: i64) (tree: []t): (i64, t) =
  (i*2+1, tree[i*2+1])

let leftchild 't (i: i64) (tree: []t): (i64, t) =
  (i*2, tree[i*2])

let parent 't (i: i64) (tree: []t): (i64, t) =
  (i/2, tree[i/2])

-- creates a tree with 2^(d+1)-1 nodes with dummy value t
let mktree 't (d: i64) (x: t): *[]t =
  let max_num_nodes = (1 << (d+1)) - 1
  in
  replicate max_num_nodes x

-- loops through a tree for an element untill it reaches a leaf
-- x: element to predict value
-- tree: decision tree returned from training
let predict (x: []f32) (tree: [](i64, f32, bool, i64)) (start: i64) : f32 =
  let (_, res, _) =
    loop (i, value, at_node)=(start, 0, true) while at_node do
      let (d, v, missing_flag, child) = tree[i]
      in
      if child >= 0 then
        if x[d] < v || (f32.isnan x[d] && missing_flag) then
          (child, value, at_node)
        else
          (child+1, value, at_node)
      else
        (i, v, false)
  in
  res

-- loops through a tree for an element untill it reaches a leaf
-- x: element to predict value
-- tree: decision tree returned from training
let predict_bin (x: []u16) (tree: [](i64, f32, bool, i64)) (b: i64) (start: i64): f32 =
  let nan_bin = u16.i64 b-1
  let (_, res, _) =
    loop (i, _, at_node)=(start, 0, true) while at_node do
      let (d, v, missing_flag, child) = tree[i]
      
      in
      if child >= 0 then
      let value = f32.u16 x[d]
      in
        if value < v || x[d] == nan_bin && missing_flag then
       --if x[d] < v || (x[d] == f32.nan && missing_flag) then
          (child, value, at_node)
        else
          (child+1, value, at_node)
      else
        (i, v, false)
  in
  res
let predict_all [n][d][l][m] (data: [n][d]f32) (trees: [m](i64,f32,bool,i64)) (offsets: [l]i64)
                             (bias: f32) : [n]f32 =
  let pred_trees = map (\x -> map (\i -> predict x trees i) offsets |> f32.sum) data
  in
  map (+bias) pred_trees
      
-- -- loops through a tree for an element untill it reaches a leaf
-- -- x: element to predict value
-- -- tree: decision tree returned from training
-- let predict (x: []f32) (tree: [](i64, f32, bool, bool)) : f32 =
--   let (_, res, _) =
--     loop (i, value, at_node)=(1, 0, true) while at_node do
--       let (d, v, missing_flag, flag) = tree[i-1]
--       in
--       if flag then
--         if x[d] < v || (f32.isnan x[d] && missing_flag) then
--           (i*2, value, at_node)
--         else
--           (i*2+1, value, at_node)
--       else
--         (i, v, flag)
--   in
--   res

-- -- loops through a tree for an element untill it reaches a leaf
-- -- x: element to predict value
-- -- tree: decision tree returned from training
-- let predict_bin (x: []u16) (tree: [](i64, f32, bool, bool)) (b: i64): f32 =
--   let nan_bin = u16.i64 b-1
--   let (_, res, _) =
--     loop (i, _, at_node)=(1, 0, true) while at_node do
--       let (d, v, missing_flag, flag) = tree[i-1]
      
--       in
--       if flag then
--       let value = f32.u16 x[d]
--       in
--         if value < v || x[d] == nan_bin && missing_flag then
--        --if x[d] < v || (x[d] == f32.nan && missing_flag) then
--           (i*2, value, at_node)
--         else
--           (i*2+1, value, at_node)
--       else
--         (i, v, flag)
--   in
--   res



-- let predict_all [n][d][l][m] (data: [n][d]f32) (trees: [m][l](i64,f32,bool,bool))
--                              (bias: f32) : [n]f32 =
--   let pred_trees = map (\x -> map (\t -> predict x t) trees |> f32.sum) data
--   in
--   map (+ bias) pred_trees

-- loops through a tree for an element untill it reaches a leaf
-- x: element to predict value
-- tree: decision tree returned from training
let predict_log (x: []f32) (tree: [](i64, f32, bool, bool)) : f32 =
  let (_, res, _) =
    loop (i, value, at_node)=(1, 0, true) while at_node do
      let (d, v, missing_flag, flag) = tree[i-1]
      in
      if flag then
        if x[d] < v || (x[d] == f32.nan && missing_flag) then
          (i*2, value, at_node)
        else
          (i*2+1, value, at_node)
      else
        (i, v, flag)
  in
  res
      

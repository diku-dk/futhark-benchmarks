-- gradient calculation for sqaured error
let gradient_mse (pred: f32) (orig: f32): f32 = pred-orig


-- hessian calculation for sqaured error
let hessian_mse (_: f32) (_: f32): f32 =  1.0


let sigmoid (x: f32) : f32 = f32.exp x/(f32.exp x+1.0)

-- gradient calcualtion for logistic
let gradient_log (pred: f32) (orig: f32) = sigmoid(pred) - orig
--t(ŷ )(1−t(ŷ )).

-- hessians calculation for logistic
let hessian_log (pred: f32) (_: f32) =
  let temp = sigmoid (pred)
  in
  --temp*(1-temp)
  f32.max (temp*(1.0-temp)) 0.000000001f32 -- abs? fmaxf

-- Returns sqaured error between label and prediction  --(label-pred)**2
let squared_error [n] (labels: [n]f32) (preds: [n]f32) : f32 =
  let err = map2 (-) labels preds |> map (**2)
  in reduce (+) 0f32 err --|> f32.sqrt
     |> (/(f32.i64 n)) |> f32.sqrt



--let AUC_error [n] (labels: [n]bool) (preds: [n]bool) = 

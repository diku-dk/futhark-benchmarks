-- Benchmarks for the reduce SOAC.

-- ==
-- entry: sum_iota_i32 sum_iota_f32 sum_iota_f64
-- input { 10000 }
-- input { 100000 }
-- input { 1000000 }
-- input { 10000000 }
-- input { 100000000 }

entry sum_iota_i32 = iota >-> i32.sum
entry sum_iota_f32 = iota >-> map r32 >-> f32.sum
entry sum_iota_f64 = iota >-> map r64 >-> f64.sum

-- ==
-- entry: sum_i32 sum_f32 sum_f64
-- random input { [10000]i32 }
-- random input { [100000]i32 }
-- random input { [1000000]i32 }
-- random input { [10000000]i32 }
-- random input { [100000000]i32 }

entry sum_i32 = i32.sum
entry sum_f32 = map r32 >-> f32.sum
entry sum_f64 = map r64 >-> f64.sum

-- Now for some non-commutative reductions.

-- ==
-- entry: prod_iota_mat4_i32 prod_iota_mat4_f32 prod_iota_mat4_f64
-- input { 10000 }
-- input { 100000 }
-- input { 1000000 }
-- input { 10000000 }
-- input { 100000000 }

-- ==
-- entry: prod_mat4_i32 prod_mat4_f32 prod_mat4_f64
-- random input { [10000]i32 [10000]i32 [10000]i32 [10000]i32 }
-- random input { [100000]i32 [100000]i32 [100000]i32 [100000]i32 }
-- random input { [1000000]i32 [1000000]i32 [1000000]i32 [1000000]i32 }
-- random input { [10000000]i32 [10000000]i32 [10000000]i32 [10000000]i32 }

let mat4_mul (+) (*) (a0,a1,a2,a3) (b0,b1,b2,b3) =
  ((b0*a0 + b1*a2),
   (b0*a1 + b1*a3),
   (b2*a0 + b3*a2),
   (b2*a1 + b3*a3))

let mat4 x = (x, x, x, x)
let mat4' f a b c d = (f a, f b, f c, f d)

entry prod_iota_mat4_i32 = iota >-> map mat4 >-> reduce (mat4_mul (i32.+) (i32.*)) (1, 0, 0, 1)
entry prod_iota_mat4_f32 = iota >-> map (r32 >-> mat4) >-> reduce (mat4_mul (f32.+) (f32.*)) (1, 0, 0, 1)
entry prod_iota_mat4_f64 = iota >-> map (r64 >-> mat4) >-> reduce (mat4_mul (f64.+) (f64.*)) (1, 0, 0, 1)

entry prod_mat4_i32 as bs cs ds = map4 (mat4'  id) as bs cs ds |> reduce (mat4_mul (i32.+) (i32.*)) (1, 0, 0, 1)
entry prod_mat4_f32 as bs cs ds = map4 (mat4' r32) as bs cs ds |> reduce (mat4_mul (f32.+) (f32.*)) (1, 0, 0, 1)
entry prod_mat4_f64 as bs cs ds = map4 (mat4' r64) as bs cs ds |> reduce (mat4_mul (f64.+) (f64.*)) (1, 0, 0, 1)

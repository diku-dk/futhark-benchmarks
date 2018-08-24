-- BFAST
-- ==
-- compiled input @ data/fake.in
-- output @ data/fake.out

import "/futlib/math"
import "/futlib/array"

import "lib/github.com/athas/vector/vector"

type real = f64
let zero : real = 0.0
let rfloor = f64.floor
let i32conv= t64
let int2real= r64
type T     = f32
let toReal (x: T) : real = f32.to_f64 x

--type real = f32
--let zero : real = 0.0
--let rfloor = f32.floor
--let i32conv= t32
--let int2real= r32
--type T     = f32
--let toReal (x: T) : real = x

let myfun (s: i32) (scale: real) (offset: real) (pt: real) : ([4]real,[4]i32) =
    let t  = (pt - offset) / scale
    let t  = t - (rfloor t)
    let t2 = t*t
    let t3 = t2*t
    let x = [-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3]

    let t  = i32conv ( (pt-offset) / scale )
    let px = [ i32.min (i32.max (t - 1i32) 0i32) (s - 1i32)
             , i32.max (i32.min t (s-1)) 0
             , i32.min (i32.max (t+1) 0) (s-1)
             , i32.max (i32.min (t+2) (s-1)) 0
             ]
    in  (x, px)

let run [sx][sy][sz][dims1][N]
        -- dim_img is replaced with sx,sy,sz 
        -- this should be made parametric
        -- but in this implementation is always 3-tuple
        -- constructor arguments
        (dataI  : [dims1][sz][sy][sx]T)
        (scale  : [3]real)
        (offset : [3]real) 
        -- run arguments
        (pts: [3][N]real) : [dims1][N]real =
    let ss = [sx, sy, sz] in
    transpose <|
      map (\i ->
            let (xyz, pxyz) = unzip (map4 myfun ss scale offset (pts[0:3,i])) in
            map (\m ->
                  let pres =
                    map (\ind ->
                            let j    = ind / 16
                            let tmp  = ind % 16
                            let k    = tmp / 4
                            let l    = tmp % 4
                            let dfdp = unsafe (xyz[0,l] * xyz[1,k] * xyz[2,j]) / 216.0
                            let dd   = unsafe dataI[m, pxyz[2,j], pxyz[1,k], pxyz[0,l]]
                            in  dfdp * (toReal dd)
                        ) (iota 64)
                  in  reduce (+) 0.0 pres
                ) (iota dims1)
          ) (iota N)

let run1 [sx][sy][sz][dims1][N]
        -- dim_img is replaced with sx,sy,sz 
        -- this should be made parametric
        -- but in this implementation is always 3-tuple
        -- constructor arguments
        (dataI:   [dims1][sz][sy][sx]T)
        (scale  : [3]real)
        (offset : [3]real) 
        -- run arguments
        (pts: [3][N]real) : [dims1][N]real =

    let (offset_x, offset_y, offset_z) = (offset[0], offset[1], offset[2])
    let (scale_x,  scale_y,  scale_z)  = ( scale[0],  scale[1],  scale[2])
    let result = 
    map (\i ->
            let res= replicate 2 <| replicate 3 <| replicate 4 zero
            let res = copy res
            let t  = (pts[0,i]-offset_x)/scale_x-rfloor((pts[0,i]-offset_x)/scale_x)
            let t2 = t*t
            let t3 = t2*t
            let res[0,0,0:4] = [-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3]

            let t  = i32conv ( (pts[0,i]-offset_x) / scale_x )
            let res[1,0,0:4] = 
                     [ int2real <| i32.min (i32.max (t - 1i32) 0i32) (sx - 1i32)
                     , int2real <| i32.max (i32.min t (sx-1)) 0
                     , int2real <| i32.min (i32.max (t+1) 0) (sx-1)
                     , int2real <| i32.max (i32.min (t+2) (sx-1)) 0
                     ]

            let t  = (pts[1,i]-offset_y)/scale_y-rfloor((pts[1,i]-offset_y)/scale_y)
            let t2 = t * t
            let t3 = t2* t
            let res[0,1,0:4] = [-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3]

            let t = i32conv (rfloor((pts[1,i]-offset_y)/scale_y))
            let res[1,1,0:4] = 
                     [ int2real <| i32.min (i32.max (t-1) 0) (sy-1)
                     , int2real <| i32.max (i32.min t (sy-1)) 0
                     , int2real <| i32.min (i32.max (t+1) 0) (sy-1)
                     , int2real <| i32.max (i32.min (t+2) (sy-1)) 0
                     ]

            let t  = (pts[2,i]-offset_z)/scale_z-rfloor((pts[2,i]-offset_z)/scale_z)
            let t2 = t*t
            let t3 = t2*t
            let res[0,2,0:4]  = [-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1, t3]

            let t  = i32conv (rfloor ((pts[2,i]-offset_z) / scale_z))
            let res[1,2,0:4] = 
                     [ int2real <| i32.min (i32.max(t-1) 0) (sz-1)
                     , int2real <| i32.max (i32.min t (sz-1)) 0
                     , int2real <| i32.min (i32.max (t+1) 0) (sz-1)
                     , int2real <| i32.max (i32.min (t+2) (sz-1)) 0
                     ]
            in
            map (\m ->
                  let pres =
                    map (\ind ->
                            let j    = ind / 16
                            let tmp  = ind % 16
                            let k    = tmp / 4
                            let l    = tmp % 4
                            let (x_l, y_k, z_j)  = unsafe
                                (res[0,0,l], res[0,1,k], res[0,2,j])
                            let dfdp = (x_l * y_k * z_j) / 216.0
                            let (px_l, py_k, pz_j) = unsafe
                                (i32conv res[1,0,l], i32conv res[1,1,k], i32conv res[1,2,j])
                            let dd   = unsafe dataI[m, pz_j, py_k, px_l]
                            in  dfdp * (toReal dd)
                        ) (iota 64)
                  in  reduce (+) 0.0 pres
                ) (iota dims1)
        ) (iota N)
    in transpose result

let run2 [sx][sy][sz][dims1][N]
        (dataI:   [dims1][sz][sy][sx]T)
        (scale  : [3]real)
        (offset : [3]real)
        (pts: [3][N]real) : [dims1][N]real =
    let res0 = replicate dims1 zero in
    transpose <|
    map (\i ->
            let (x, px) = myfun sx (scale[0]) (offset[0]) (pts[0,i])
            let (y, py) = myfun sy (scale[1]) (offset[1]) (pts[1,i])
            let (z, pz) = myfun sz (scale[2]) (offset[2]) (pts[2,i])
            let res = copy res0 in
            loop(res) for m < dims1 do
                let s = zero in
                let res[m] = 
                    loop(s) for j < 4 do
                     loop(s) for k < 4 do
                      loop(s) for l < 4 do
                        let dfdp = (x[l] * y[k] * z[j]) / 216.0
                        let dd   = unsafe dataI[m, pz[j], py[k], px[l]]
                        in  s + dfdp * (toReal dd)
                in  res
        ) (iota N)

module vector_2 = cat_vector vector_1 vector_1
module v4 = cat_vector vector_2 vector_2
type vec4 't = v4.vector t

let myfunVct (s: i32) (scale: real) (offset: real) (pt: real) : 
             (vec4 real, vec4 i32) =
    let t  = (pt - offset) / scale
    let t  = t - (rfloor t)
    let t2 = t*t
    let t3 = t2*t
    let x = v4.from_array <|
            [-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3]

    let t  = i32conv ( (pt-offset) / scale )
    let px = v4.from_array <|
             [ i32.min (i32.max (t - 1i32) 0i32) (s - 1i32)
             , i32.max (i32.min t (s-1)) 0
             , i32.min (i32.max (t+1) 0) (s-1)
             , i32.max (i32.min (t+2) (s-1)) 0
             ]
    in  (x, px)


let run3 [sx][sy][sz][dims1][N]
        (dataI:   [dims1][sz][sy][sx]T)
        (scale  : [3]real)
        (offset : [3]real)
        (pts: [3][N]real) : [dims1][N]real =
    let res0 = replicate dims1 zero in
    transpose <|
    map (\i ->
            let (x, px) = myfunVct sx (scale[0]) (offset[0]) (pts[0,i])
            let (y, py) = myfunVct sy (scale[1]) (offset[1]) (pts[1,i])
            let (z, pz) = myfunVct sz (scale[2]) (offset[2]) (pts[2,i])
            let res = copy res0 in
            loop(res) for m < dims1 do
              let res[m] = 
                v4.reduce (+) zero <|
                v4.map (\j ->
                    let (z_j, pz_j) = (v4.get j z, v4.get j pz) in
                    v4.reduce (+) zero <|
                    v4.map (\k ->
                        let (y_k, py_k) = (v4.get k y, v4.get k py) in
                        v4.reduce (+) zero <|
                        v4.map (\l -> let (x_l, px_l) = (v4.get l x, v4.get l px)
                                      let dfdp = (x_l * y_k * z_j) / 216.0
                                      let dd   = unsafe dataI[m, pz_j, py_k, px_l]
                                      in  dfdp * (toReal dd)
                               ) (v4.from_array <| iota 4)
                           ) (v4.from_array <| iota 4)
                       ) (v4.from_array <| iota 4)
              in res
        ) (iota N)

let run4 [sx][sy][sz][dims1][N]
        (dataI:   [dims1][sz][sy][sx]T)
        (scale  : [3]real)
        (offset : [3]real)
        (pts: [3][N]real) : [dims1][N]real =
    let res0 = replicate dims1 zero in
    transpose <|
    map (\i ->
            let (x, px) = myfunVct sx (scale[0]) (offset[0]) (pts[0,i])
            let (y, py) = myfunVct sy (scale[1]) (offset[1]) (pts[1,i])
            let (z, pz) = myfun sz (scale[2]) (offset[2]) (pts[2,i])

            let res = copy res0 in
            loop(res) for m < dims1 do
              let s = zero
              let res[m] = 
                    loop(s) for j < 4 do
                      let (z_j, pz_j) = (z[j], pz[j]) in s +
                      (v4.reduce (+) zero <|
                       v4.map (\k -> 
                            let (y_k, py_k) = (v4.get k y, v4.get k py)
                            in v4.reduce (+) zero <|
                               v4.map (\l -> let (x_l, px_l) = (v4.get l x, v4.get l px)
                                             let dfdp = (x_l * y_k * z_j) / 216.0
                                             let dd   = unsafe dataI[m, pz_j, py_k, px_l]
                                             in  dfdp * (toReal dd)
                                      ) (v4.from_array <| iota 4)
                              ) (v4.from_array <| iota 4)
                      )
              in res
        ) (iota N)


let main [sx][sy][sz][dims1][N]
         (dataI:   [dims1][sz][sy][sx]T)
         (scale0  : real)
         (offset0 : real) 
         -- run arguments
         (pts: [3][N]real) : [dims1][N]real =
    let scale = [scale0,  scale0,  scale0]
    let offset= [offset0, offset0, offset0]
    in  run2 dataI scale offset pts

-- futhark-dataset -b --f32-bounds=0.0:1.0 -g [1][200][200][200]f32 --f64-bounds=1:1 -g f64 --f64-bounds=0:0 -g f64 --f64-bounds=1.0:199.0 -g [3][1000000]f64 > fake.in
-- futhark-dataset -b --f32-bounds=0.0:1.0 -g [1][200][200][200]f32 --f32-bounds=1:1 -g f32 --f32-bounds=0:0 -g f32 --f32-bounds=1.0:199.0 -g [3][1000000]f32 > fake32.in
-- gcc -o interp_cos_plays interp_cos_plays.c -lOpenCL -lm -O3 -std=c9
-- futhark-pkg add github.com/athas/vector


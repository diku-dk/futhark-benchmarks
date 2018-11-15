-- BFAST
-- ==
-- compiled input @ data/fake.in
-- output @ data/fake.out

type real = f64
let zero : real = 0.0
let rfloor = f64.floor
let i32conv= t64
type T     = f32
let toReal (x: T) : real = f32.to_f64 x

-- make it main if you want to look at the
-- dataset.
let main0 [sx][sy][sz][dims1][N]

        (dataI:   [dims1][sz][sy][sx]T)
        (_scale0  : real)
        (_offset0 : real)
        (pts: [3][N]real) : 
        ([dims1][sz][sy][sx]T, [3][N]real) =

    (dataI, pts)


let run [sx][sy][sz][dims1][N]
        -- dim_img is replaced with sx,sy,sz 
        -- this should be made parametric
        -- but in this implementation is always 3-tuple
        -- constructor arguments
        (dataI:   [dims1][sz][sy][sx]T)
        (scale  : (real,real,real))
        (offset : (real,real,real)) 
        -- run arguments
        (pts: [3][N]real) : [dims1][N]real =

    let (offset_x, offset_y, offset_z) = offset
    let (scale_x,  scale_y,  scale_z)  = scale
    let result = 
    map (\i ->
            let t  = (pts[0,i]-offset_x)/scale_x-rfloor((pts[0,i]-offset_x)/scale_x)
            let t2 = t*t
            let t3 = t2*t
            let x  = [-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3]

            let t  = i32conv ( (pts[0,i]-offset_x) / scale_x )
            let px = [ i32.min (i32.max (t - 1i32) 0i32) (sx - 1i32)
                     , i32.max (i32.min t (sx-1)) 0
                     , i32.min (i32.max (t+1) 0) (sx-1)
                     , i32.max (i32.min (t+2) (sx-1)) 0
                     ]

            let t  = (pts[1,i]-offset_y)/scale_y-rfloor((pts[1,i]-offset_y)/scale_y)
            let t2 = t * t
            let t3 = t2* t
            let y = [-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1,  t3]

            let t = i32conv (rfloor((pts[1,i]-offset_y)/scale_y))
            let py = [ i32.min (i32.max (t-1) 0) (sy-1)
                     , i32.max (i32.min t (sy-1)) 0
                     , i32.min (i32.max (t+1) 0) (sy-1)
                     , i32.max (i32.min (t+2) (sy-1)) 0
                     ]

            let t  = (pts[2,i]-offset_z)/scale_z-rfloor((pts[2,i]-offset_z)/scale_z)
            let t2 = t*t
            let t3 = t2*t
            let z  = [-t3+3*t2-3*t+1, 3*t3-6*t2+4, -3*t3+3*t2+3*t+1, t3]

            let t  = i32conv (rfloor ((pts[2,i]-offset_z) / scale_z))
            let pz = [ i32.min (i32.max(t-1) 0) (sz-1)
                     , i32.max (i32.min t (sz-1)) 0
                     , i32.min (i32.max (t+1) 0) (sz-1)
                     , i32.max (i32.min (t+2) (sz-1)) 0
                     ]
            in
            map (\m ->
                  let pres =
                    map (\ind ->
                            let j    = ind / 16
                            let tmp  = ind % 16
                            let k    = tmp / 4
                            let l    = tmp % 4
                            let dfdp = unsafe (x[l] * y[k] * z[j]) / 216.0
                            let dd   = unsafe dataI[m, pz[j], py[k], px[l]]
                            in  dfdp * (toReal dd)
                        ) (iota 64)
                  in  reduce (+) 0.0 pres
                ) (iota dims1)
        ) (iota N)
    in transpose result

let main [sx][sy][sz][dims1][N]
         (dataI:   [dims1][sz][sy][sx]T)
         (scale0  : real)
         (offset0 : real) 
         -- run arguments
         (pts: [3][N]real) : [dims1][N]real =
    let scale = (scale0,  scale0,  scale0 )
    let offset= (offset0, offset0, offset0)
    in  run dataI scale offset pts

-- futhark-dataset -b --f32-bounds=0.0:1.0 -g [1][200][200][200]f32 --f64-bounds=1:1 -g f64 --f64-bounds=0:0 -g f64 --f64-bounds=1.0:199.0 -g [3][1000000]f64 > fake.in


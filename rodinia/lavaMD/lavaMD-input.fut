default(f32)

fun num_neighbors(): int      = 27
fun number_par_per_box(): int = 100

------------------------------------------
-- Util: Sobol random number generation --
------------------------------------------
fun sobolDirVcts(): [30]int = 
  [ 536870912, 268435456, 134217728, 67108864, 33554432, 16777216, 8388608, 4194304, 2097152, 1048576, 
    524288,    262144,    131072,    65536,    32768,    16384,    8192,    4096,    2048,    1024, 
    512,       256,       128,       64,       32,       16,       8,       4,       2,       1      ] 

fun sobolInd(dirVct:  [30]int, n: int ): int =
  let n_gray = (n >> 1) ^ n
  let res = 0
  loop (res) =
    for i < 30 do
      let t = 1 << i
      in if (n_gray & t) == t
         then res ^ dirVct[i]
         else res
  in res

-----------------------
-----------------------
fun main(boxes1d: int): (f32,
                         []i32,
                         []i32,
                         []i32,
                         []i32,

                         [][]i32,
                         [][]i32,
                         [][]i32,
                         [][]i32,

                         []i32,

                         [][]f32,
                         [][]f32,
                         [][]f32,
                         [][]f32,

                         [][]f32) =
  let number_boxes = boxes1d * boxes1d * boxes1d
  let alpha        = 0.5
  let num_nn       = num_neighbors()
  let par_per_box  = number_par_per_box()
  let dirVct       = sobolDirVcts()

  ----------------------------------------
  -- 1. Initialize boxs' data structure --
  ----------------------------------------
  let boxes = 
    map (fn (nh: int): ( (int, int, int, int), [num_nn](int,int,int,int), int )  =>
          let k = nh % boxes1d
          let nr= nh / boxes1d
          let j = nr % boxes1d
          let i = nr / boxes1d
          
          -- current home box
          let box_coef = ( k, j, i, nh )
          -- initialize neighbor boxes
          let box_nngh_cur_nn = (replicate num_nn (0,0,0,0), 0)
          loop (box_nngh_cur_nn) =
            for nn < num_nn do
              let (box_nngh, cur_nn) = box_nngh_cur_nn
              let n = (nn % 3) - 1
              let nr= nn / 3
              let m = (nr % 3) - 1
              let l = (nr / 3) - 1
              let (cur_elem, next_cur_nn) = 
                -- check if (this neighbor exists) and (it is not the same as home box)
                if( (( 0<=(i+l) && 0<=(j+m) && 0<=(k+n))               &&
                     ((i+l)<boxes1d && (j+m)<boxes1d && (k+n)<boxes1d)) &&
                   (!(l==0 && m==0 && n==0))                             )
                then  let (x, y, z) = (k+n, j+m, i+l)
                      let number = (z * boxes1d * boxes1d) + (y * boxes1d) + x
                      in ( (x, y, z, number), cur_nn+1)
                else  ( (0, 0, 0, 0     ), cur_nn  ) 
          in unsafe
          let box_nngh[cur_nn] = cur_elem
         in (box_nngh, next_cur_nn)

          let (box_nngh, cur_nn) = box_nngh_cur_nn
          in  ( box_coef, box_nngh, cur_nn )

       ) (iota(number_boxes) )

  let (box_coefs, box_nnghs0, box_num_nghbs) = unzip(boxes)
  let box_nnghs = copy(transpose(box_nnghs0))

  ----------------------------------------------
  -- 2. Initialize input distances and charge --
  ----------------------------------------------
  let rqv = map  (fn (i: int): [par_per_box](f32,(f32,f32,f32,f32))  =>
                    map (fn (j: int): (f32, (f32,f32,f32,f32))  =>
                           let n = (i*par_per_box + j)*5 + 1
                           let s1= sobolInd(dirVct, n  )
                           let s2= sobolInd(dirVct, n+1)
                           let s3= sobolInd(dirVct, n+2)
                           let s4= sobolInd(dirVct, n+3)
                           let s5= sobolInd(dirVct, n+4)
                           in (f32(s5%10 + 1) / 10.0, 
                               ( f32(s1%10 + 1) / 10.0, f32(s2%10 + 1) / 10.0
                               , f32(s3%10 + 1) / 10.0, f32(s4%10 + 1) / 10.0 )
                              )
                       ) (iota(par_per_box))
                ) (iota(number_boxes) )

  let (qv, rv) = unzip(rqv)

  in ( alpha,

       #0(unzip(box_coefs)),
       #1(unzip(box_coefs)),
       #2(unzip(box_coefs)),
       #3(unzip(box_coefs)),

       #0(unzip(box_nnghs)),
       #1(unzip(box_nnghs)),
       #2(unzip(box_nnghs)),
       #3(unzip(box_nnghs))

       box_num_nghbs,

       #0(unzip(rv)),
       #1(unzip(rv)),
       #2(unzip(rv)),
       #3(unzip(rv)),

       qv)

default(f32)

fun int num_neighbors()      = 27
fun int number_par_per_box() = 100

------------------------------------------
-- Util: Sobol random number generation --
------------------------------------------
fun [30]int sobolDirVcts() = 
  [ 536870912, 268435456, 134217728, 67108864, 33554432, 16777216, 8388608, 4194304, 2097152, 1048576, 
    524288,    262144,    131072,    65536,    32768,    16384,    8192,    4096,    2048,    1024, 
    512,       256,       128,       64,       32,       16,       8,       4,       2,       1      ] 

fun int sobolInd( [30]int dirVct, int n ) =
  let n_gray = (n >> 1) ^ n in
  let res = 0 in
  loop (res) =
    for i < 30 do
      let t = 1 << i in
      if (n_gray & t) == t
      then res ^ dirVct[i]
      else res
  in res

-----------------------
-----------------------
fun (f32,
     [](i32, i32, i32, i32),
     [][](i32, i32, i32, i32),
     []i32,
     [][](f32,f32,f32,f32),
     [][]f32)
  --fun [][](int,int,int,int)
  main(int boxes1d) =
  let number_boxes = boxes1d * boxes1d * boxes1d in
  let alpha        = 0.5                  in
  let num_nn       = num_neighbors()      in
  let par_per_box  = number_par_per_box() in
  let dirVct       = sobolDirVcts()       in

  ----------------------------------------
  -- 1. Initialize boxs' data structure --
  ----------------------------------------
  let boxes = 
    map(fn ( (int, int, int, int), [num_nn](int,int,int,int), int ) (int nh) =>
          let k = nh % boxes1d in
          let nr= nh / boxes1d in
          let j = nr % boxes1d in
          let i = nr / boxes1d in
          
          -- current home box
          let box_coef = ( k, j, i, nh ) in
          -- initialize neighbor boxes
          let box_nngh_cur_nn = (replicate(num_nn, (0,0,0,0)), 0) in
          loop (box_nngh_cur_nn) =
            for nn < num_nn do
              let (box_nngh, cur_nn) = box_nngh_cur_nn in 
              let n = (nn % 3) - 1 in
              let nr= nn / 3       in
              let m = (nr % 3) - 1 in
              let l = (nr / 3) - 1 in
              let (cur_elem, next_cur_nn) = 
                -- check if (this neighbor exists) and (it is not the same as home box)
                if( (( 0<=(i+l) && 0<=(j+m) && 0<=(k+n))               &&
                     ((i+l)<boxes1d && (j+m)<boxes1d && (k+n)<boxes1d)) &&
                   (!(l==0 && m==0 && n==0))                             )
                then  let (x, y, z) = (k+n, j+m, i+l) in
                      let number = (z * boxes1d * boxes1d) + (y * boxes1d) + x in
                      ( (x, y, z, number), cur_nn+1)
                else  ( (0, 0, 0, 0     ), cur_nn  ) 
          in unsafe
          let box_nngh[cur_nn] = cur_elem in 
          (box_nngh, next_cur_nn)
        in
             let (box_nngh, cur_nn) = box_nngh_cur_nn
             in  ( box_coef, box_nngh, cur_nn )

       , iota(number_boxes) )
  in
  let (box_coefs, box_nnghs0, box_num_nghbs) = unzip(boxes)  in
  let box_nnghs = copy(transpose(box_nnghs0)) in

  ----------------------------------------------
  -- 2. Initialize input distances and charge --
  ----------------------------------------------
  let rqv = map ( fn [par_per_box](f32,(f32,f32,f32,f32)) (int i) =>
                    map( fn (f32, (f32,f32,f32,f32)) (int j) =>
                           let n = (i*par_per_box + j)*5 + 1 in
                           let s1= sobolInd(dirVct, n  ) in 
                           let s2= sobolInd(dirVct, n+1) in 
                           let s3= sobolInd(dirVct, n+2) in
                           let s4= sobolInd(dirVct, n+3) in 
                           let s5= sobolInd(dirVct, n+4) in
                           (   f32(s5%10 + 1) / 10.0, 
                               ( f32(s1%10 + 1) / 10.0, f32(s2%10 + 1) / 10.0
                               , f32(s3%10 + 1) / 10.0, f32(s4%10 + 1) / 10.0 )
                           )
                       , iota(par_per_box))
                , iota(number_boxes) )
  in
  let (qv, rv) = unzip(rqv)  in
  ----------------------------------------------
  -- 3. Finally, call the computational kernel--
  ----------------------------------------------
  ( alpha, box_coefs, box_nnghs, box_num_nghbs, rv, qv )

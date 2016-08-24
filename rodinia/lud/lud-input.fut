default(f32)

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
fun [n][n]f32 main(int n) =
    let divisor = 2.0 ** f32(30) in
    let sobvcts = sobolDirVcts()
    let mat = map( fn f32 (int ind) =>
                        let x = sobolInd(sobvcts, ind+1) in
                        f32(x) / divisor
                 , iota(n*n))
    in reshape((n,n), mat)


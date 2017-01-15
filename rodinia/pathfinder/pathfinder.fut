-- Code and comments based on
-- https://github.com/kkushagra/rodinia/blob/master/openmp/pathfinder/
--
-- ==
--
-- notravis input @ data/medium.in
-- output @ data/medium.out

fun in_range   (x: i32, lb: i32, ub: i32): bool = (x >= lb) && (x <= ub)
fun clamp_range(x: i32, lb: i32, ub: i32): i32 = if      (x < lb) then lb 
                                              else if (x > ub) then ub else x
fun min(a: i32, b: i32): i32 = if (a <= b) then a else b


------------------------------------------
-- Util: Sobol random number generation --
------------------------------------------
fun sobolDirVcts(): [30]i32 = 
    [ 536870912, 268435456, 134217728, 67108864, 33554432, 16777216, 8388608, 4194304, 2097152, 1048576, 
      524288,    262144,    131072,    65536,    32768,    16384,    8192,    4096,    2048,    1024, 
      512,       256,       128,       64,       32,       16,       8,       4,       2,       1      ] 

fun sobolInd(dirVct:  [30]i32, n: i32 ): i32 =
    let n_gray = (n >> 1) ^ n
    let res = 0
    loop (res) =
      for i < 30 do
        let t = 1 << i
        in if (n_gray & t) == t
           then res ^ dirVct[i]
           else res
    in res

fun main(cols: i32, rows: i32): [cols]i32 =
    let dirVct = sobolDirVcts()
    -----------------------
    -- 1. Initialization --
    -----------------------
    let wall_flat = 
        map (\(i: i32): i32  -> 
                sobolInd(dirVct, i+1) % 10
           ) (iota(rows*cols) )

    let wall   = reshape (rows,cols) wall_flat
    let result = copy(wall[0])
    
    ---------------
    -- 1. Kernel --
    ---------------
    loop (result) = for t < (rows-1) do
        map (\(i: i32): i32  ->
                let res = result[i]
                let res = if (i >  0)     then min(res, unsafe result[i-1]) else res

                let res = if (i < cols-1) then min(res, unsafe result[i+1]) else res

                in wall[t+1, i] + res 
           ) (iota(cols) )
    in result

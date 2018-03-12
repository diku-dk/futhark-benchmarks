-- Parallel blocked LU-decomposition.
--
-- ==
-- input @ data/16by16.in
-- output @ data/16by16.out
-- compiled input @ data/64.in
-- output @ data/64.out
-- compiled input @ data/256.in
-- output @ data/256.out
-- compiled input @ data/512.in
-- output @ data/512.out
-- compiled input @ data/2048.in
-- output @ data/2048.out

import "/futlib/array"

-------------------------------------------
---- Translation of: lud_diagonal_omp -----
-------------------------------------------
----
---- limitted parallelism, cannot efficiently
---- parallelize in the manner of rodinia-cuda
---------------------------------------------------------------------
---------------------------------------------------------------------

let lud_diagonal0 [b] (a: [b][b]f32): *[b][b]f32 =  -- CORRECT
    let a_cols = copy(transpose(a)) in
    let a_rows = copy(a) in
    let (a_rows,a_cols) = loop (a_rows,a_cols) for i < b do
        let it_row = map (\ (j: i32): f32  ->
                            if j < i then 0.0f32 else
                            let sum = 0.0f32 in
                            let sum = loop sum for k < i do
                                sum + a_cols[k,i]*a_rows[k,j]
                            in  a_rows[i,j]-sum
                        ) (iota(b) )
        in
        let it_col = map (\ (j: i32): f32  ->
                            if j < (i+1) then 0.0f32 else
                            let sum = 0.0f32 in
                            let sum = loop sum for k < i do
                                sum + a_cols[k,j]*a_rows[k,i]
                            in  (a_cols[i,j]-sum) / it_row[i]
                        ) (iota(b) )
        in
        let a_rows[i] = it_row in
        let a_cols[i] = it_col in
        (a_rows,a_cols)
    in map (\ a_rows_r a_cols_r (i: i32): [b]f32  ->
                    map (\ (row_el: f32) (col_el: f32) (j: i32): f32  ->
                                if (i <= j) then row_el else col_el
                           ) (a_rows_r) (a_cols_r) (iota(b) )
              ) (a_rows) (transpose(a_cols)) (iota(b) )

let lud_diagonal1 [b] (a: [b][b]f32): *[b][b]f32 =  -- CORRECT
    let a_cols = copy(transpose(a)) in
    let b2 = 2*b in
    let a_rc = map (\ (i: i32): [b2]f32  ->
                        map (\ (j: i32): f32  ->
                                if j < b
                                then unsafe a[i,j  ]
                                else unsafe a_cols[i,j-b]
                           ) (iota(b2) )
                  ) (iota(b) )
    let a_rc = loop a_rc for i < (b-1) do
        let it_rc  = map (\ (j: i32): f32  ->
                            if (j < b) -- row case
                            then let i = i + 1 in
                                 if j < i then 0.0f32 else
                                 --let sum = a[0,j]*a_rc[k,b+i+1] in
                                 let sum = loop sum=0.0f32 for k < i do
                                    sum + a_rc[k,b+i-1]*(if k==0 then a[0,j] else a_rc[k-1,j])
                                 in  a_rc[i,j]-sum
                            else let j = j - b in -- column case
                                 if j < (i+1) then 0.0f32 else
                                 let sum = loop sum=0.0f32 for k < i do
                                    sum + a_rc[k,b+j] * (if k==0 then a[0,i] else a_rc[k-1,i])
                                 in  (a_rc[i,b+j]-sum) / (if i==0 then a[i,i] else a_rc[i-1,i]) --it_row[i]
                        ) (iota(b2) )
        in
        let a_rc[i] = it_rc in
        a_rc
    in
    let (a_rows0, a_cols0) = unzip(
            map (\ (ind: i32): (f32,f32)  ->
                    let i = ind / b in let j = ind % b in unsafe
                    let r_el = if i == 0 then a[i,j] else a_rc[i-1,j] in
                    (r_el, a_rc[i,j+b])
               ) (iota(b*b) )
        ) in
    let (a_rows, a_cols) = ( reshape (b,b) a_rows0, reshape (b,b) a_cols0 )
    in map (\ (a_rows_r: [b]f32) (a_cols_r: [b]f32) (i: i32): [b]f32  ->
                    map (\ (row_el: f32) (col_el: f32) (j: i32): f32  ->
                                if (i <= j) then row_el else col_el
                           ) (a_rows_r) (a_cols_r) (iota(b) )
              ) (a_rows) (transpose a_cols) (iota b)


let lud_diagonal2 [b] (ain: [b][b]f32, m: i32): *[b][b]f32 =  -- CORRECT
    let one = (m*m+2*m+1)/(m+1) - m in
    let ains= copy(replicate one ain) in
    let ress= map (\ (a: *[b][b]f32, q: i32): *[b][b]f32  -> unsafe
                     loop a for i < b do
                        let a = loop a for j in i..<b do
                            let sum = loop sum=0.0f32 for k < i do
                                sum + a[i,k]*a[k,j] + r32(q)
                            let a[i,j] = a[i,j] - sum
                            in a
                        let tmp = 1.0f32 / a[i,i] in
                        loop a for j in i+1..<b do
                            let sum = loop sum=0.0f32 for k < i do
                                sum + a[j,k] * a[k,i]
                            let a[j,i] = (a[j,i] - sum) * tmp
                            in a
                 ) (zip ains (iota(one)) )
    in reshape (b,b) ress


let lud_diagonal [b] (a: [b][b]f32): *[b][b]f32 =  -- CORRECT
    let a_cols = copy(transpose(a)) in
    let b2 = 2*b in
    let a_rc = map (\ (i: i32): [b2]f32  ->
                        map (\ (j: i32): f32  ->
                                if j < b
                                then unsafe a[i,j  ]
                                else unsafe a_cols[i,j-b]
                           ) (iota(b2) )
                  ) (iota(b) )
    let a_rc = loop a_rc for i < b do
        let row_col =
            map (\ (j: i32): f32  ->
                    if j < b
                    then
                        if j < i then 0.0f32 else
                        let sum = loop sum=0.0f32 for k < i do
                            sum + a_rc[k,i+b]*a_rc[k,j]
                        in  a_rc[i,j]-sum
                    else
                        let j = j - b in
                        if j < (i+1) then 0.0f32 else
                        let aii = loop aii=a_rc[i,i] for k < i do
                            aii - (a_rc[k,i+b]*a_rc[k,i])
                        in
                        let sum = loop sum=0.0f32 for k < i do
                            sum + a_rc[k,j+b]*a_rc[k,i]
                        in  (a_rc[i,j+b]-sum) / aii
               ) (iota(b2) )
        in
        let a_rc[i] = row_col in
        a_rc
    in map (\ (i: i32): [b]f32  ->
            map (\ (j: i32): f32  ->
                    if (i <= j) then a_rc[i,j] else a_rc[j,i+b]
               ) (iota(b) )
          ) (iota(b) )
------------------------------
------------------------------
---- LUD Perimeter Upper -----
------------------------------
------------------------------

let lud_perimeter_upper [m][b] (diag: [b][b]f32, a0s: [m][b][b]f32): *[m][b][b]f32 =
    let a1s = map (\ (x: [b][b]f32): [b][b]f32  -> transpose(x)) a0s in
    let a2s =
        map  (\a1: [b][b]f32  ->
              map  (\row0: [b]f32  ->   -- Upper
                    loop row=replicate b 0.0f32 for i < b do
                    let sum = (loop sum=0.0f32 for k < i do sum + diag[i,k] * row[k])
                    let row[i] = row0[i] - sum
                    in  row
                   ) a1
             ) a1s
    in map (\x: [b][b]f32 -> transpose(x)) a2s


------------------------------
------------------------------
---- LUD Perimeter Lower -----
------------------------------
------------------------------

let lud_perimeter_lower [b][m] (diag: [b][b]f32, mat: [m][m][b][b]f32): *[m][b][b]f32 =
  let slice = mat[0:m,0] in
  map  (\blk: [b][b]f32  ->
        map  (\ (row0: [b]f32): *[b]f32  ->   -- Lower
                loop row=replicate b 0.0f32 for j < b do
                        let sum = loop sum=0.0f32 for k < j do
                            sum + diag[k,j] * row[k]
                        let row[j] = (row0[j] - sum) / diag[j,j]
                        in  row
            ) blk
      ) slice


------------------------------
------------------------------
----     LUD Internal    -----
------------------------------
------------------------------

let lud_internal [mp1][b] (top_per: [mp1][b][b]f32, lft_per: [mp1][b][b]f32, mat: [mp1][mp1][b][b]f32 ): *[][][b][b]f32 =
  let top_slice0= top_per[1:mp1] in
  let top_slice = rearrange (0,2,1) top_slice0 in
  let lft_slice = lft_per[1:mp1] in
  let mat_slice = mat[1:mp1,1:mp1] in
  map (\[m] (mat_arr: [m][b][b]f32, lft: [b][b]f32): [m][b][b]f32  ->
        map (\ (mat_blk: [b][b]f32, top: [b][b]f32): [b][b]f32  ->
                map  (\ (mat_row: [b]f32, lft_row: [b]f32): [b]f32  ->
                        map  (\(mat_el, top_row)  ->
                                let prods = map (*) (lft_row) (top_row) in
                                let sum   = reduce (+) (0.0f32) prods     in
                                mat_el - sum
                             ) (zip (mat_row) top)
                    ) (zip (mat_blk) lft )
           ) (zip (mat_arr) (top_slice) )
     ) (zip (mat_slice) (lft_slice) )

let block_size = 32

--------------------------------------------
---- Main Driver:
--------------------------------------------
let main [m] (mat: [m][m]f32): [m][m]f32 =
    let b = block_size
    let num_blocks = (m+b-1) / b -- rounding up
    let n = b * num_blocks
    -- Maybe pad the input to be a multiple of the block size.
    let padding = n - m
    let mat = if padding != 0
              then concat (map (\r -> concat r (replicate padding 0f32)) mat)
                          (replicate padding (replicate n 0f32))
              else mat
    -------------------------------------------------
    ---- transform matrix in [n/b,n/b,b,b] block ----
    ---- versions for upper and lower parts      ----
    ---- the blocks of the lower part            ----
    -------------------------------------------------
    let matb =
        map  (\ (i_b: i32): [num_blocks][b][b]f32  ->
                map  (\ (j_b: i32): [b][b]f32  ->
                        map (\ (i: i32): [b]f32  ->
                                map  (\ (j: i32): f32  ->
                                        unsafe mat[i_b*b+i, j_b*b + j]
                                    ) (iota(b) )
                           ) (iota(b) )
                    ) (iota(num_blocks) )
            ) (iota(num_blocks) )
    in
    let upper = copy(matb) in
    let lower = copy(rearrange (1,0,2,3) matb) in
    --------------------------------------
    ---- sequential tiled loop driver ----
    --------------------------------------
    let (upper,lower,matb) = loop((upper,lower,matb)) for step < ((n / b) - 1) do
        -----------------------------------------------
        ---- 1. compute the current diagonal block ----
        -----------------------------------------------
        let diag = lud_diagonal(matb[0,0]) in
        --let upper[step,step] = diag in
        ----------------------------------------
        ---- 2. compute the top  perimeter  ----
        ----------------------------------------
        let top_per_irreg = lud_perimeter_upper(diag, matb[0]) in
        let top_per_all =
            map  (\ (ind: i32): f32  ->
                    let jj = ind / (b*b) in
                    let tmp= ind % (b*b) in
                    let i  = tmp / b     in
                    let j  = tmp % b     in
                    if (jj < step)
                    then unsafe upper[step,jj,i,j]
                    else if jj == step
                         then unsafe diag[i,j]
                         else unsafe top_per_irreg[jj-step,i,j]
                ) (iota(num_blocks*b*b) ) in
        let upper[step] = reshape (num_blocks,b,b) top_per_all
        in
        ----------------------------------------
        ---- 3. compute the left perimeter  ----
        ----    and update matrix           ----
        ----------------------------------------
        let lft_per_irreg = lud_perimeter_lower(diag, matb) in
        let lft_per_all =
            map  (\ (ind: i32): f32  ->
                    let ii = ind / (b*b) in
                    let tmp= ind % (b*b) in
                    let i  = tmp / b     in
                    let j  = tmp % b     in
                    if (ii <= step)
                    then unsafe lower[step,ii,i,j]
                    else unsafe lft_per_irreg[ii-step,i,j]
                ) (iota(num_blocks*b*b) ) in
        let lower[step] = reshape (num_blocks,b,b) lft_per_all
        in
        ----------------------------------------
        ---- 4. compute the internal blocks ----
        ----------------------------------------
        let matb = lud_internal(top_per_irreg, lft_per_irreg, matb)
        in (upper,lower,matb)
    ---------------------
    -- LOOP ENDS HERE! --
    ---------------------
    in
    let last_step = (n / b) - 1 in
    let upper[last_step,last_step] =
      lud_diagonal( reshape (b,b) matb ) in
    let ret_padded = map (\(i_ind: i32): [n]f32  ->
                          map  (\ (j_ind: i32): f32  ->
                                let (ii, jj) = (i_ind/b, j_ind/b) in
                                let ( i,  j) = (i_ind - ii*b, j_ind - jj*b) in
                                if (ii <= jj)
                                then unsafe upper[ii,jj,i,j]
                                else unsafe lower[jj,ii,i,j]
                               ) (iota n)
                         ) (iota n)
    in take m (map (take m) ret_padded)

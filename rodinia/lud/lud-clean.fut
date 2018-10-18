-- Parallel blocked LU-decomposition.
--
-- ==
-- input @ data/16by16.in
-- output @ data/16by16.out
-- compiled input @ data/64.in
-- output @ data/64.out
-- compiled input @ data/256.in
-- output @ data/256.out
-- compiled input @ data/512.in.gz
-- output @ data/512.out.gz
-- compiled input @ data/2048.in.gz
-- output @ data/2048.out.gz

-------------------------------------------
---- Translation of: lud_diagonal_omp -----
-------------------------------------------
----
---- limitted parallelism, cannot efficiently
---- parallelize in the manner of rodinia-cuda
---------------------------------------------------------------------
---------------------------------------------------------------------

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

let lud_perimeter_lower [b][m] (diag: [b][b]f32, mat: [m][b][b]f32): *[m][b][b]f32 =
  map (\blk: [b][b]f32  ->
        map  (\ (row0: [b]f32): *[b]f32  ->   -- Lower
                loop row=replicate b 0.0f32 for j < b do
                        let sum = loop sum=0.0f32 for k < j do
                            sum + diag[k,j] * row[k]
                        let row[j] = (row0[j] - sum) / diag[j,j]
                        in  row
            ) blk
      ) mat


------------------------------
------------------------------
----     LUD Internal    -----
------------------------------
------------------------------

let lud_internal [m][b] (top_per: [m][b][b]f32, lft_per: [m][b][b]f32, mat_slice: [m][m][b][b]f32 ): *[m][m][b][b]f32 =
  let top_slice = map transpose top_per in
  map (\[m] (mat_arr: [m][b][b]f32, lft: [b][b]f32): [m][b][b]f32  ->
        map (\ (mat_blk: [b][b]f32, top: [b][b]f32): [b][b]f32  ->
                map  (\ (mat_row: [b]f32, lft_row: [b]f32): [b]f32  ->
                        map  (\(mat_el, top_row)  ->
                                let prods = map2 (*) lft_row top_row
                                let sum   = f32.sum prods
                                in mat_el - sum
                             ) (zip (mat_row) top)
                    ) (zip (mat_blk) lft )
           ) (zip (mat_arr) (top_slice) )
     ) (zip (mat_slice) (lft_per) )

let block_size: i32 = 16

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
              then map (++replicate padding 0f32) mat ++
                   replicate padding (replicate n 0f32)
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

    --------------------------------------
    ---- sequential tiled loop driver ----
    --------------------------------------
    let matb = loop(matb) for step < ((n / b) - 1) do
        -----------------------------------------------
        ---- 1. compute the current diagonal block ----
        -----------------------------------------------
        let diag = lud_diagonal(matb[step,step]) in

        ----------------------------------------
        ---- 2. compute the top  perimeter  ----
        ----------------------------------------
        let row_slice = matb[step,step+1:num_blocks]
        let top_per_irreg = lud_perimeter_upper(diag, row_slice)
        
        ----------------------------------------
        ---- 3. compute the left perimeter  ----
        ----    and update matrix           ----
        ----------------------------------------
        let col_slice = matb[step+1:num_blocks,step]
        let lft_per_irreg = lud_perimeter_lower(diag, col_slice)
        
        ----------------------------------------
        ---- 4. compute the internal blocks ----
        ----------------------------------------
        let inner_slice = matb[step+1:num_blocks,step+1:num_blocks]
        let internal = lud_internal(top_per_irreg, lft_per_irreg, inner_slice)

        ----------------------------------------
        ---- 5. update matrix in place      ----
        ----------------------------------------
        let matb[step,step] = diag
        let matb[step, step+1:num_blocks] = top_per_irreg
        let matb[step+1:num_blocks, step] = lft_per_irreg
        let matb[step+1:num_blocks, step+1:num_blocks] = internal
        
        in matb
    ---------------------
    -- LOOP ENDS HERE! --
    ---------------------

    let last_step = (n / b) - 1 in
    let matb[last_step,last_step] = 
            lud_diagonal( matb[last_step, last_step] )

    let ret_padded = map (\(i_ind: i32): [n]f32  ->
                          map  (\ (j_ind: i32): f32  ->
                                let (ii, jj) = (i_ind/b, j_ind/b)
                                let ( i,  j) = (i_ind - ii*b, j_ind - jj*b)
                                in  unsafe matb[ii,jj,i,j]
                               ) (iota n)
                         ) (iota n)
    in take m (map (take m) ret_padded)

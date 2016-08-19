-- Sequential LU-decomposition.
--
-- ==
-- tags { nobench }
-- compiled input @ data/16by16.in
-- output @ data/16by16.out


-------------------------------------------
---- Translation of: lud_diagonal_omp -----
-------------------------------------------
----
---- limitted parallelism, cannot efficiently 
---- parallelize in the manner of rodinia-cuda
---------------------------------------------------------------------
---------------------------------------------------------------------

fun *[b][b]f32
lud_diagonal([b][b]f32 a) =  -- CORRECT
    let a_cols = copy(transpose(a)) in
    let a_rows = copy(a) in
    loop( (a_rows,a_cols) ) = for i < b do
        let it_row = map( fn f32 (int j) =>
                            if j < i then 0.0f32 else
                            let sum = 0.0f32 in
                            loop(sum) = for k < i do
                                sum + a_cols[k,i]*a_rows[k,j]
                            in  a_rows[i,j]-sum
                        , iota(b) )
        in
        let it_col = map( fn f32 (int j) =>
                            if j < (i+1) then 0.0f32 else
                            let sum = 0.0f32 in
                            loop(sum) = for k < i do
                                sum + a_cols[k,j]*a_rows[k,i]
                            in  (a_cols[i,j]-sum) / it_row[i]
                        , iota(b) )
        in
        let a_rows[i] = it_row in
        let a_cols[i] = it_col in
        (a_rows,a_cols)
    in zipWith( fn [b]f32 ([b]f32 a_rows_r, [b]f32 a_cols_r, int i) =>
                    zipWith( fn f32 (f32 row_el, f32 col_el, int j) =>
                                if (i <= j) then row_el else col_el
                           , a_rows_r, a_cols_r, iota(b) )                 
              , a_rows, transpose(a_cols), iota(b) )

------------------------------
------------------------------
---- LUD Perimeter Upper -----
------------------------------
------------------------------
----
---- Ideally: diag is hold in CONSTANT memory!
----          row = a1s[jj] is in shared memory!
--------------------------------------------
--------------------------------------------
fun *[m][b][b]f32
lud_perimeter_upper0(int step, [b][b]f32 diag, [m][b][b]f32 a0s) = copy(a0s)

fun *[m][b][b]f32
lud_perimeter_upper(int step, [b][b]f32 diag, [m][b][b]f32 a0s) =
    let a1s = map(fn [b][b]f32 ([b][b]f32 x) => transpose(x), a0s) in
    let a2s = 
        map ( fn *[b][b]f32 ([b][b]f32 a1, int jj) =>
        map ( fn *[b]f32 ([b]f32 row0) =>   -- Upper
                loop(row=copy(row0)) = for im1 < b - 1 do
                    let i   = im1 + 1 in
                    let sum = 0.0f32  in
                    loop(sum) = for k < i do
                        sum + diag[i,k] * row[k]
                    in 
                    let row[i] = row[i] - sum
                    in  row
                in row
            , a1 )
            , zip(a1s,iota(m)) )
    in map(fn [b][b]f32 ([b][b]f32 x) => transpose(x), a2s)

------------------------------
------------------------------
---- LUD Perimeter Lower -----
------------------------------
------------------------------
----
---- Ideally: diag is hold in CONSTANT memory!
----          row = mat[i,step] is in shared memory!
--------------------------------------------
--------------------------------------------
fun *[m][b][b]f32
lud_perimeter_lower0(int step, [b][b]f32 diag, [m][m][b][b]f32 mat) = copy(mat[0])


fun *[m][b][b]f32
lud_perimeter_lower(int step, [b][b]f32 diag, [m][m][b][b]f32 mat) =
  map ( fn *[b][b]f32 ([m][b][b]f32 mat_row, int ii) =>
        let blk = mat_row[0] in
        map ( fn *[b]f32 ([b]f32 row0) =>   -- Lower
                loop(row=copy(row0)) = for j < b do
                        loop(sum=0.0f32) = for k < j do
                            sum + diag[k,j] * row[k]
                        in
                        let row[j] = (row[j] - sum) / diag[j,j]
                        in  row
                in row
            , blk )
      , zip(mat,iota(m)) )

------------------------------
------------------------------
----     LUD Internal    -----
------------------------------
------------------------------
----
---- Ideally: temp_top and temp_left
----          are stored in shared memory!!!
--------------------------------------------
--------------------------------------------

fun *[][][b][b]f32
lud_internal( int d, [mp1][b][b]f32 top_per, [mp1][b][b]f32 lft_per, [mp1][mp1][b][b]f32 mat ) =
  let m = mp1 - 1 in
  map( fn [m][b][b]f32 (int ii) =>
        map( fn [b][b]f32 (int jj) =>
               let top = top_per[jj+1] in 
               let lft = lft_per[ii+1] in 
                map ( fn [b]f32 (int i) =>
                        map ( fn f32 (int j) =>
                                loop (sum = 0.0f32) = for k < b do
                                         sum + lft[i,k] * top[k,j]
                                in mat[ii+1,jj+1,i,j] - sum
                                
                            , iota(b) )
                    , iota(b) )
           , iota(m) )
     , iota(m) )


fun *[][][b][b]f32
lud_internal1( int d, [mp1][b][b]f32 top_per0t, [mp1][b][b]f32 lft_per0, [mp1][mp1][b][b]f32 mat ) =
  let m = mp1 - 1 in
  let (tmp,top_per0) = split((1),top_per0t) in
  let top_per = rearrange((0,2,1), top_per0) 
  let (tmp,lft_per) = split((1),lft_per0) in
  map( fn [m][b][b]f32 ([b][b]f32 lft, int ii) =>
        map( fn [b][b]f32 ([b][b]f32 top, int jj) =>
                map ( fn [b]f32 ([b]f32 lft_row, int i) =>
                        map ( fn f32 ([b]f32 top_row, int j) =>
                                let prods = zipWith(*, lft_row, top_row)     in
                                let sum   = reduce(+, 0.0f32, prods) in
                                mat[ii+1,jj+1,i,j] - sum
                                
                            , zip(top,iota(b)) )
                    , zip(lft,iota(b)) )
           , zip(top_per,iota(m)) )
     , zip(lft_per,iota(m)) )


--------------------------------------------
---- Main Driver:
--------------------------------------------
fun [n][n]f32 main([n][n]f32 mat) =
    let b = 16 in -- 16 in
    let num_blocks = n / b in
    -------------------------------------------------
    ---- transform matrix in [n/b,n/b,b,b] block ----
    ---- versions for upper and lower parts      ----
    ---- the blocks of the lower part            ----
    -------------------------------------------------
    let matb = 
        map ( fn [num_blocks][b][b]f32 (int i_b) =>
                map ( fn [b][b]f32 (int j_b) =>
                        map( fn [b]f32 (int i) =>
                                map ( fn f32 (int j) =>
                                        unsafe mat[i_b*b+i, j_b*b + j]
                                    , iota(b) )
                           , iota(b) )
                    , iota(num_blocks) )
            , iota(num_blocks) )
    in
    let upper = copy(matb) in
    let lower = copy(rearrange((1,0,2,3),matb)) in
    --------------------------------------
    ---- sequential tiled loop driver ----
    --------------------------------------
    loop((upper,lower,matb)) = for step < ((n / b) - 1) do
        -----------------------------------------------
        ---- 1. compute the current diagonal block ----
        -----------------------------------------------
        let diag = lud_diagonal(matb[0,0]) in
        --let upper[step,step] = diag in
        ----------------------------------------
        ---- 2. compute the top  perimeter  ----
        ----------------------------------------
        let top_per_irreg = lud_perimeter_upper(step, diag, matb[0]) in
        let top_per_all = 
            map ( fn f32 (int ind) => 
                    let jj = ind / (b*b) in
                    let tmp= ind % (b*b) in
                    let i  = tmp / b     in
                    let j  = tmp % b     in
                    if (jj < step) 
                    then unsafe upper[step,jj,i,j]
                    else if jj == step 
                         then unsafe diag[i,j]
                         else unsafe top_per_irreg[jj-step,i,j]
                , iota(num_blocks*b*b) ) in
        let upper[step] = reshape((num_blocks,b,b),top_per_all)
        in
        ----------------------------------------
        ---- 3. compute the left perimeter  ----
        ----    and update matrix           ----
        ----------------------------------------
        let lft_per_irreg = lud_perimeter_lower(step, diag, matb) in
        let lft_per_all = 
            map ( fn f32 (int ind) => 
                    let ii = ind / (b*b) in
                    let tmp= ind % (b*b) in
                    let i  = tmp / b     in
                    let j  = tmp % b     in
                    if (ii <= step) 
                    then unsafe lower[step,ii,i,j]
                    else unsafe lft_per_irreg[ii-step,i,j]
                , iota(num_blocks*b*b) ) in
        let lower[step] = reshape((num_blocks,b,b),lft_per_all)
        in
        ----------------------------------------
        ---- 4. compute the internal blocks ----
        ----------------------------------------
        let matb = lud_internal( step, top_per_irreg, lft_per_irreg, matb )
        in (upper,lower,matb)
    ---------------------
    -- LOOP ENDS HERE! --
    ---------------------
    in
    let last_step = (n / b) - 1 in
    let upper[last_step,last_step] = 
            lud_diagonal( reshape((b,b),matb) ) in
    map ( fn [n]f32 (int i_ind) =>
            map ( fn f32 (int j_ind) =>
                    let (ii, jj) = (i_ind/b, j_ind/b) in
                    let ( i,  j) = (i_ind - ii*b, j_ind - jj*b) in
                    if (ii <= jj) 
                    then unsafe upper[ii,jj,i,j]
                    else unsafe lower[jj,ii,i,j]
                , iota(n) )
        , iota(n) )
    

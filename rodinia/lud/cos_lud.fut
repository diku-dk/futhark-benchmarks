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
                let row = copy(row0) in
                if (jj <= step) -- move this if inside!!!
                then row
                else
                -- copy row?
                loop(row) = for im1 < b - 1 do
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
  map ( fn *[b][b]f32 ([m][b][b]f32 a0s, int ii) =>
        map ( fn *[b]f32 (int i) =>   -- Lower
                let row = copy(unsafe a0s[step,i]) in
                -- copy row?
                loop(row) = for j < b do
                    --if (ii <= step) -- move this if inside!
                    --then row
                    --else
                        loop(sum=0.0f32) = for k < j do
                            sum + diag[k,j] * row[k]
                        in
                        let row[j] = (row[j] - sum) / diag[j,j]
                        in  row
                in row
            , iota(b) )
      , zip(mat,iota(m)) )

fun *[m][b][b]f32
lud_perimeter_lower1(int step, [b][b]f32 diag, [m][m][b][b]f32 mat) =
  map ( fn *[b][b]f32 ([m][b][b]f32 a0s, int ii) =>
        let a0 = unsafe a0s[step] in
        map ( fn *[b]f32 ([b]f32 row0) =>   -- Lower
                let row = copy(row0) in
                if (ii <= step) -- move this if inside!
                then row
                else
                -- copy row?
                loop(row) = for j < b do
                    let sum = 0.0f32  in
                    loop(sum) = for k < j do
                        sum + diag[k,j] * row[k]
                    in
                    let row[j] = (row[j] - sum) / diag[j,j]
                    in  row
                in row
            , a0 )
      , zip(mat,iota(m)) )

fun *[m][b][b]f32
lud_perimeter_lower2(int step, [b][b]f32 diag, [m][m][b][b]f32 mat) =
  map ( fn *[b][b]f32 ([m][b][b]f32 a0s, int ii) =>
        map ( fn *[b]f32 (int i) =>   -- Lower
                let row = copy(a0s[step,i]) in
                loop(row) = for j < b do
                    if (ii <= step) 
                    then row
                    else loop(sum=0.0f32) = for k < j do
                            sum + diag[k,j] * row[k]
                         in
                         let row[j] = (row[j] - sum) / diag[j,j]
                         in  row
                in row
            , iota(b) )
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
fun *[m][m][b][b]f32
lud_internal0( int d, [m][m][b][b]f32 mat ) =
  map( fn [m][b][b]f32 (int ii) =>
        map( fn [b][b]f32 (int jj) =>
                let top = transpose(mat[d, jj]) in 
                let left= mat[ii, d] in 
                map ( fn [b]f32 ([b]f32 left_row, int i) =>
                        map ( fn f32 ([b]f32 top_row, int j) =>
                                if (ii > d && jj > d)
                                then -- let prods = zipWith(*, left_row, top_row) in
                                     -- let sum   = reduce (+, 0.0f32, prods) in
                                     -- mat[ii,jj,i,j] - sum
                                      loop (sum = 0.0f32) = for k < b do
                                         sum + left_row[k] * top_row[k]
                                      in mat[ii,jj,i,j] - sum
                                else mat[ii,jj,i,j]
                            , zip(top,iota(b)) )
                    , zip(left,iota(b)) )
           , iota(m) )
     , iota(m) )

fun *[m][m][b][b]f32
lud_internal( int d, [m][m][b][b]f32 mat ) =
  map( fn [m][b][b]f32 (int ii) =>
        map( fn [b][b]f32 (int jj) =>
                let top = mat[d, jj] in 
                let left= mat[ii, d] in 
                map ( fn [b]f32 (int i) =>
                        map ( fn f32 (int j) =>
                                if (ii > d && jj > d)
                                then loop (sum = 0.0f32) = for k < b do
                                         sum + left[i,k] * top[k,j]
                                      in mat[ii,jj,i,j] - sum
                                else mat[ii,jj,i,j]
                            , iota(b) )
                    , iota(b) )
           , iota(m) )
     , iota(m) )


--------------------------------------------
---- Main Driver:
--------------------------------------------
fun [n][n]f32 main([n][n]f32 mat) =
    let b = 2 in -- 16 in
    let num_blocks = n / b in
    -------------------------------------------------
    ---- transform matrix in [n/b,n/b,b,b] block ----
    ---- versions for upper and lower parts      ----
    ---- the blocks of the lower part            ----
    -------------------------------------------------
    let upp_blk = 
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
    --------------------------------------
    ---- sequential tiled loop driver ----
    --------------------------------------
    loop(upp_blk) = for step < ((n / b) - 1) do
        -----------------------------------------------
        ---- 1. compute the current diagonal block ----
        -----------------------------------------------
        let diag = lud_diagonal(upp_blk[step,step]) in
        let upp_blk[step,step] = diag in
        ----------------------------------------
        ---- 2. compute the top  perimeter  ----
        ----------------------------------------
        let top_per =
            lud_perimeter_upper(step, diag, upp_blk[step]) in
        let upp_blk[step] = top_per
        in
        ----------------------------------------
        ---- 3. compute the left perimeter  ----
        ----    and update matrix           ----
        ----------------------------------------
        let left_per = 
            lud_perimeter_lower(step, diag, upp_blk) in
        let upp_blk = 
            map (fn [num_blocks][b][b]f32 ([num_blocks][b][b]f32 upp_arr_blk, [b][b]f32 left_blk, int ii) =>
                    map (fn [b][b]f32 ([b][b]f32 upp_blk, int jj) =>
                            map (fn [b]f32 ([b]f32 upp_row, [b]f32 left_row, int i) =>
                                    map (fn f32 (f32 upp_el, f32 left_el, int j) =>
                                            if(ii > step) && (jj == step)
                                            then left_el--left_per[ii,i,j]
                                            else upp_el--upp_blk[ii,jj,i,j]
                                        , zip(upp_row,left_row,iota(b)) )
                                , zip(upp_blk, left_blk, iota(b)) )
                        , zip(upp_arr_blk, iota(num_blocks)) )
                , zip(upp_blk,left_per,iota(num_blocks)) )
        in
        ----------------------------------------
        ---- 4. compute the internal blocks ----
        ----------------------------------------
        lud_internal( step, upp_blk )
    ---------------------
    -- LOOP ENDS HERE! --
    ---------------------
    in
    let last_step = (n / b) - 1 in
    let upp_blk[last_step,last_step] = 
            lud_diagonal(upp_blk[last_step,last_step]) in
    map ( fn [n]f32 (int i) =>
            map ( fn f32 (int j) =>
                let (ii_new, jj_new) = (i/b, j/b) in
                let ( i_new,  j_new) = (i - ii_new*b, j - jj_new*b) in 
                unsafe upp_blk[ii_new, jj_new, i_new, j_new] --upp_blk[i/b, j/b, i%b, j%b]
                , iota(n) )
        , iota(n) )
    

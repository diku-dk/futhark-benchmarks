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
lud_diagonal0([b][b]f32 a0) = -- CORRECT
    let a = copy(a0) in
    loop(a) = for i < b do
        loop(a) = for i <= j < b do
            loop(a) = for k < i do
                let a[i,j] = a[i,j] - a[i,k]*a[k,j] 
                in  a
            in a
        in
        let tmp = 1.0f32 / a[i,i] in
        loop(a) = for (i+1) <= j < b do
            loop(a) = for k < i do
                let a[j,i] = a[j,i] - a[j,k]*a[k,i]
                in  a
            in 
            let a[j,i] = a[j,i]*tmp
            in  a
        in a
    in a

fun *[b][b]f32
lud_diagonal1([b][b]f32 a0) =  -- CORRECT
    let a = copy(a0) in
    loop(a) = for i < b do
        let (vals, inds) = unzip( 
            map ( fn (f32,int) (int j) =>
                    if j < i then (0.0f32, -1) else
                    let sum = 0.0f32 in
                    loop(sum) = for k < i do
                        sum + a[i,k]*a[k,j]
                    in (a[i,j]-sum, i*b+j)
                , iota(b) ) )
        in
        let a = reshape((b*b), a) in
        let a = write(inds, vals, a) in
        let a = reshape((b,b), a) in
        let (vals, inds) = unzip( 
            map ( fn (f32,int) (int j) =>
                    if j < (i+1) then (0.0f32, -1) else
                    let sum = 0.0f32 in
                    loop(sum) = for k < i do
                        sum + a[j,k]*a[k,i]
                    in ( (a[j,i]-sum)/a[i,i], j*b+i)
                , iota(b) ) )
        in
        let a = reshape((b*b), a) in
        let a = write(inds, vals, a) in
        reshape((b,b), a)
    in a

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


fun *[m][b][b]f32
lud_perimeter_upper1(int step, [b][b]f32 diag, [m][b][b]f32 a0s) =
    let a1s = rearrange((0,2,1), a0s) in
    let a2s = 
        map ( fn *[b][b]f32 ([b][b]f32 a1, int jj) =>
        map ( fn *[b]f32 ([b]f32 row, int j) =>   -- Upper
        map ( fn f32 ([b]f32 diag_row, f32 el, int i) => 
                if (jj <= step) 
                then el
                else
                    let sum = 0.0f32  in
                    let (diag_c, _) = split((i), diag_row) in
                    let (row_c , _) = split((i), row ) in
                    let prods = zipWith(*, diag_c, row_c) in
                    let sum   = reduce(+, 0.0f32, prods) in
                    el - sum
            , zip(diag, row, iota(b)) )
            , zip(a1, iota(b)) )
            , zip(a1s,iota(m)) )
    in rearrange((0,2,1), a2s)

fun *[m][b][b]f32
lud_perimeter_upper2(int step, [b][b]f32 diag, [m][b][b]f32 a0s) =
    let a2s = 
        map ( fn *[b][b]f32 ([b][b]f32 a0, int jj) =>
        map ( fn *   [b]f32 (int j) =>
                if jj <= step
                then map(fn f32 (int i) => a0[i,j], iota(b))
                else let a1 = transpose(a0) in
                     let row= a1[j] in
                     map(fn f32 ([b]f32 diag_i, int i)  =>
                            let (diag_c, _) = split((i), diag_i) in
                            let (row_c , _) = split((i), row ) in
                            let prods = zipWith(*, diag_c, row_c) in
                            let sum   = reduce(+, 0.0f32, prods) in
                            row[i] - sum
                        , zip(diag,iota(b)) )
            , iota(b) )
            , zip(a0s, iota(m)) )
    in rearrange((0,2,1), a2s)

fun *[m][b][b]f32
lud_perimeter_upper3(int step, [b][b]f32 diag, [m][b][b]f32 a0ss) = 
        map ( fn *[b][b]f32 ([b][b]f32 a0s, int jj) =>
        map ( fn *[b]f32 ([b]f32 a0, int i) =>   -- Upper
        map ( fn f32 (f32 el, int j) => 
                if (jj <= step) 
                then el
                else
                    let a1t = transpose(a0s) in
                    let row = unsafe a1t[j] in
                    let (row_c , _) = split((i), row ) in
                    let (diag_c, _) = split((i), unsafe diag[i]) in
                    let prods = zipWith(*, diag_c, row_c) in
                    let sum   = reduce (+, 0.0f32, prods) in
                    el - sum
            , zip(a0,  iota(b)) )
            , zip(a0s, iota(b)) )
            , zip(a0ss,iota(m)) )


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
lud_perimeter_lower(int step, [b][b]f32 diag, [m][m][b][b]f32 mat) =
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
lud_perimeter_lower0(int step, [b][b]f32 diag0, [m][m][b][b]f32 mat) = -- COMPILER BUG??? when setting b = 2
  let diag = transpose(diag0) in
  map ( fn *[b][b]f32 ([m][b][b]f32 a0s, int ii) =>
        let a0 = unsafe a0s[step] in
        map ( fn *[b]f32 ([b]f32 row) => -- Lower
        map ( fn f32 (f32 el, int j) =>
                if (ii <= step) 
                then el
                else
                    let (row_c , _) = split((j), row    ) in
                    let (diag_c, _) = split((j), diag[j]) in
                    let prods = zipWith(*, diag_c, row_c) in
                    let sum   = reduce (+, 0.0f32, prods) in
                    (el - sum) / diag0[j,j]
            , zip(row,iota(b)) )
            , a0 )
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
lud_internal( int d, [m][m][b][b]f32 mat ) =
  map( fn [m][b][b]f32 (int ii) =>
        map( fn [b][b]f32 (int jj) =>
                let temp_top = mat[d, jj] in -- copy?
                let temp_left= mat[ii, d] in -- copy?
                map ( fn [b]f32 (int i) =>
                        map ( fn f32 (int j) =>
                                if (ii > d && jj > d)
                                then let sum = 0.0f32 in
                                     loop (sum) = for k < b do
                                        sum + temp_left[i,k] * temp_top[k,j]
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
    let b = 4 in -- 16 in
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
            map (fn [num_blocks][b][b]f32 (int ii) =>
                    map (fn [b][b]f32 (int jj) =>
                            map (fn [b]f32 (int i) =>
                                    map (fn f32 (int j) =>
                                            if(ii > step) && (jj == step)
                                            then left_per[ii,i,j]
                                            else upp_blk[ii,jj,i,j]
                                        , iota(b) )
                                , iota(b) )
                        , iota(num_blocks) )
                , iota(num_blocks) )
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
    

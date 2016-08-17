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
lud_diagonal([b][b]f32 a0) =  -- CORRECT
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
lud_diagonal3([b][b]f32 a0) = -- CORRECT
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
lud_perimeter_upper(int d, [b][b]f32 diag, [m][b][b]f32 a0s) =
    let a1s = map(fn [b][b]f32 ([b][b]f32 x) => transpose(x), a0s) in
    let a2s = 
        map ( fn *[b][b]f32 ([b][b]f32 a1, int jj) =>
        map ( fn *[b]f32 ([b]f32 row0) =>   -- Upper
                let row = copy(row0) in
                if (jj <= d) -- move this if inside!!!
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
                let temp_top = copy(mat[d, jj]) in -- copy?
                let temp_left= copy(mat[ii, d]) in -- copy?
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
    let b = 16 in -- 16 in
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
    

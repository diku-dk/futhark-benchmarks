-- Sequential LU-decomposition.
--
-- ==
-- tags { nobench }
-- compiled input @ data/16by16.in
-- output @ data/16by16.out



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
fun *[m][m]f32
lud_perimeter_lower(int step, f32 diag, *[m][m]f32 mat) =
  map ( fn *[m]f32 (*[m]f32 a0s, int ii) =>
            let row = unsafe a0s[step] in
            let a0s[step] = if (ii <= step) 
                            then row
                            else row / diag
            in  a0s
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
fun *[m][m]f32
lud_internal( int step, [m][m]f32 mat ) =
  map( fn [m]f32 (int ii) =>
        map( fn f32 (int jj) =>
                if (ii > step && jj > step)
                then let temp_top = mat[step, jj] in
                     let temp_left= mat[ii, step] in
                     mat[ii,jj] - temp_left*temp_top
                else mat[ii,jj]
           , iota(m) )
     , iota(m) )


--------------------------------------------
---- Main Driver:
--------------------------------------------
fun [n][n]f32 main(*[n][n]f32 mat) =
    let num_blocks = n in
    -------------------------------------------------
    ---- transform matrix in [n/b,n/b,b,b] block ----
    ---- versions for upper and lower parts      ----
    ---- the blocks of the lower part            ----
    -------------------------------------------------
    let upp_blk = transpose(mat)
    in
    --------------------------------------
    ---- sequential tiled loop driver ----
    --------------------------------------
    loop(upp_blk) = for step < (n - 1) do
        let diag = upp_blk[step,step]
        in
        ----------------------------------------
        ---- 3. compute the left perimeter  ----
        ----    and update matrix           ----
        ----------------------------------------
        let upp_blk = transpose(
            lud_perimeter_lower(step, diag, transpose(upp_blk)) )
        in
        ----------------------------------------
        ---- 4. compute the internal blocks ----
        ----------------------------------------
        lud_internal( step, upp_blk )
    ---------------------
    -- LOOP ENDS HERE! --
    ---------------------
    in transpose(upp_blk)
    

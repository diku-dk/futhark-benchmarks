
-------------------------------------------
---- Translation of: lud_diagonal_omp -----
-------------------------------------------
----
----void lud_diagonal_omp (float* a, int size, int offset) {
----    int i, j, k;
----    for (j = 0; j < BS; j++) {
----        for (i = 0; i < BS-1; i++) {
----            if(j > i)
----                float temp = 1.f/AA(i,i);
----                for (k = 0; k < i ; k++) {
----                    AA(j,i) = AA(j,i) - AA(j,k) * AA(k,i);
----                }
----                AA(j,i) = AA(j,i)*temp;
----            }
----        }
----    }
----
----    for (j = 0; j < BS; j++) {
----        for (i = 0; i < BS-1; i++) {
----            if( j > i) {
----                for (k = 0; k < i+1 ; k++) {
----                    AA(i+1,j) = AA(i+1,j) - AA(i+1,k) * AA(k,j);
----                }
----            }
----        }
----    }
----}
---------------------------------------------------------------------
---------------------------------------------------------------------
fun *[[f32,b],b]
lud_diagonal([[f32,b],b] a0) =
    let a1 = 
        map(fn [f32,b] (int j) =>
                let row = copy(a0[j]) in
                loop(row) = for i <  b - 1 do
                    let {sum, tmp} = if(j > i) 
                    then let sum = 0.0f32  in
                         loop (sum) = for k < i do
                            sum + row[k] * a0[k,i]
                         in {sum, a0[i,i]}
                    else    {0.0f32, 1.0f32}
                    in
                    let row[i] = (row[i] - sum) / tmp
                    in row
                in row
           , iota(b) )
    in
    let a2 = transpose(a1) in
    let res = 
        map(fn [f32,b] (int j) =>
                let row = copy(a2[j]) in
                loop(row) = for i <  b - 1 do
                    let sum = if(j > i) 
                    then let sum = 0.0f32 in
                         loop (sum) = for k < (i+1) do
                            sum + a2[k,i+1] * row[k]
                         in sum
                    else 0.0f32
                    in
                    let row[i+1] = row[i+1] - sum
                    in row
                in row
           , iota(b) )
    in transpose(res)



------------------------------
------------------------------
---- LUD Perimeter Upper -----
------------------------------
------------------------------
----
----for (j = 0; j < BS; j++) {
----    for (i = 0; i < BS; i++) {
----        sum = 0.f;
----        for (k=0; k < i; k++) {
----            sum += temp[BS*i +k] * BB((i_global+k),(j_global+j));
----        }
----        i_here = i_global + i;
----        j_here = j_global + j;
----        BB(i_here, j_here) = BB(i_here,j_here) - sum;
----    }
----}
----
---- Ideally: diag is hold in CONSTANT memory!
----          row = a1[j] is in shared memory!
--------------------------------------------
--------------------------------------------
fun *[[[f32,b],b],m]
lud_perimeter_upper(int d, [[f32,b],b] diag, [[[f32,b],b],m] a0s) =
    let a1s = map(fn [[f32,b],b] ([[f32,b],b] x) => transpose(x), a0s) in
    let a2s = 
        map ( fn *[[f32,b],b] ([[f32,b],b] a1, int jj) =>
        map ( fn *[f32,b] ([f32,b] row0) =>   -- Upper
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
    in map(fn [[f32,b],b] ([[f32,b],b] x) => transpose(x), a2s)

------------------------------
------------------------------
---- LUD Perimeter Lower -----
------------------------------
------------------------------
----j_global = offset;
----i_global += BS * (chunk_idx + 1);
----for (i = 0; i < BS; i++) {
----    for (j = 0; j < BS; j++) {
----        sum = 0.f;
----        for (k=0; k < j; k++) {
----            sum += BB((i_global+i),(j_global+k)) * temp[BS*k + j];
----        }
----        i_here = i_global + i;
----        j_here = j_global + j;
----        a[size*i_here + j_here] = ( a[size*i_here+j_here] - sum ) / a[size*(offset+j) + offset+j];
----    }
----}
----
---- Ideally: diag is hold in CONSTANT memory!
----          row = a0[i] is in shared memory!
--------------------------------------------
--------------------------------------------
--- FIX ME, see upper!!!!!!!!!
fun *[[[f32,b],b],m]
lud_perimeter_lower(int d, [[f32,b],b] diag, [[[f32,b],b],m] a0s) =
  map ( fn *[[f32,b],b] ([[f32,b],b] a0, int ii) =>
        map ( fn *[f32,b] ([f32,b] row0) =>   -- Upper
                let row = copy(row0) in
                if (ii <= d) -- move this if inside!
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
      , zip(a0s,iota(m)) )
------------------------------
------------------------------
----     LUD Internal    -----
------------------------------
------------------------------
----
----    for  (chunk_idx =0; chunk_idx < chunks_per_inter; chunk_idx++)
----    {
----        int i, j, k, i_global, j_global;
----        float temp_top[BS*BS] __attribute__ ((aligned (64)));
----        float temp_left[BS*BS] __attribute__ ((aligned (64)));
----        float sum[BS] __attribute__ ((aligned (64))) = {0.f};
----        
----        i_global = offset + BS * (1 +  chunk_idx/chunks_in_inter_row);
----        j_global = offset + BS * (1 + chunk_idx%chunks_in_inter_row);

----        for (i = 0; i < BS; i++) {
----#pragma omp simd
----            for (j =0; j < BS; j++){
----                temp_top[i*BS + j]  = a[size*(i + offset) + j + j_global ]; 
----                temp_left[i*BS + j] = a[size*(i + i_global) + offset + j];
----            }
----        }
----
----        for (i = 0; i < BS; i++)
----        {
----            for (k=0; k < BS; k++) {
----#pragma omp simd 
----                for (j = 0; j < BS; j++) {
----                    sum[j] += temp_left[BS*i + k] * temp_top[BS*k + j];
----                }
----            }
----#pragma omp simd 
----            for (j = 0; j < BS; j++) {
----                BB((i+i_global),(j+j_global)) -= sum[j];
----                sum[j] = 0.f;
----            }
----        }
----    }
----
---- Ideally: temp_top and temp_left
----          are stored in shared memory!!!
--------------------------------------------
--------------------------------------------
fun *[[[[f32,b],b],m],m]
lud_internal( int d, [[[[f32,b],b],m],m] mat ) =
  map( fn [[[f32,b],b],m] (int ii) =>
        map( fn [[f32,b],b] (int jj) =>
                let temp_top = copy(mat[d, jj]) in -- copy?
                let temp_left= copy(mat[ii, d]) in -- copy?
                map ( fn [f32,b] (int i) =>
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
fun [[f32,n],n]
--main(int b, [[f32,n],n] mat) =
main([[f32,n],n] mat) =
    let b = 16 in
    let num_blocks = n / b in
    -------------------------------------------------
    ---- transform matrix in [n/b,n/b,b,b] block ----
    ---- versions for upper and lower parts      ----
    ---- the blocks of the lower part            ----
    -------------------------------------------------
    let upp_blk = 
        map ( fn [[[f32,b],b],num_blocks] (int i_b) =>
                map ( fn [[f32,b],b] (int j_b) =>
                        map( fn [f32,b] (int i) =>
                                map ( fn f32 (int j) =>
                                        unsafe mat[i_b*b+i, j_b*b + j]
                                    , iota(b) )
                           , iota(b) )
                    , iota(num_blocks) )
            , iota(num_blocks) )
    in
    let low_blk = 
        map ( fn [[[f32,b],b],num_blocks] (int i_b) =>
                map ( fn [[f32,b],b] (int j_b) =>
                        map( fn [f32,b] (int i) =>
                                map ( fn f32 (int j) =>
                                        upp_blk[j_b, i_b, i, j] -- inside the tile they keep the same disposition
                                    , iota(b) )
                           , iota(b) )
                    , iota(num_blocks) )
            , iota(num_blocks) )
    in
    --------------------------------------
    ---- sequential tiled loop driver ----
    --------------------------------------
    loop(upp_blk) = for step < (n / b) - 1 do

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
            lud_perimeter_lower(step, diag, low_blk[step]) in
        let upp_blk = 
            map (fn [[[f32,b],b],num_blocks] (int ii) =>
                    map (fn [[f32,b],b] (int jj) =>
                            map (fn [f32,b] (int i) =>
                                    map (fn f32 (int j) =>
                                            if(ii <= step) || (jj != step)
                                            then upp_blk[ii,jj,i,j]
                                            else left_per[ii,i,j]
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
    map ( fn [f32,n] (int i) =>
            map ( fn f32 (int j) =>
                unsafe upp_blk[i/b, j/b, i%b, j%b]
                , iota(n) )
        , iota(n) )
    

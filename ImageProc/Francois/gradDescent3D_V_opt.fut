-- add_descent_div3v
-- ==

-- compiled input @ data_gradDescent3D_V/tiny.in
-- output @ data_gradDescent3D_V/tiny.out


----------------------------------------------------------------
--- Futhark Translation of gradient-descent@ graddiv.py file.---
--- This implements some of the gradient and divergence      ---
--- operations used in P3 Segmentation algorithms.           ---
--- This should behave as the routines of ImageArray.h       ---
--- Orignal Python by: Francois Lauze, 2014-2015             ---
--- Futhark translation: Cosmin Oancea, May, 2016            ---
--- Both from University of Copenhagen                       --- 
----------------------------------------------------------------


-----------------------------------------------------------------------
-- add_descent_div3v implements gradient descent for the primal 
-- variable in CCP splitting algorithm. 3D vector case. 
-- (i.e., a 3D CCP type type algorithm).
----
-- Arguments:
-- v : primal variable, i.e., the label array, dimension (m,n,p,q).
-- xi: dual variable, dimensions should be (m,n,p,q,3)
-- g : data term gradient array, size (m,n,p,q).
-- tp: gradient descent time step
-----------------------------------------------------------------------
let add_descent_div3v [m][n][p][q] (v:  [m][n][p][q]f32
                                   , xi: [m][n][p][q](f32,f32,f32)
                                   , g: [m][n][p][q]f32
                                   , tp: f32 ): [m][n][p][q]f32 =
  let one = (m*m+2*m+1)/(m+1) - m
  let res_flat = 
    map (\(ind: i32): f32  -> unsafe
            let tmp = ind / q
            let l   = ind - tmp*q
            let ind = tmp

            let tmp = ind / p
            let k   = ind - tmp*p
            let ind = tmp

            let tmp = ind / n
            let j   = ind - tmp*n
            let i   = tmp

                    -- get current `v`) (`g`) (and `xi` element
                    let v_el = v[i,j,k,l]
                    let g_el = g[i,j,k,l]
                    let (xi_0,xi_1,xi_2) = xi[i,j,k,l]
                    -- get the neighbors of the current `xi` element
                    let xi_100_0 = if i < 1 then 0.0f32
                                   else let (v_100_0, _, _) = xi[i-1,j,k,l] in v_100_0
                    let xi_010_1 = if j < 1 then 0.0f32
                                   else let (_, v_010_1, _) = xi[i,j-1,k,l] in v_010_1
                    let xi_001_2 = if k < 1 then 0.0f32
                                   else let (_, _, v_001_2) = xi[i,j,k-1,l] in v_001_2
                    in
                    if      (i > 0) && (i < m-1) && (j > 0) && (j < n-1) && (k > 0) && (k < p-1)
                    -- THE INTERIOR:
                        -- v[1:m-1, 1:n-1, 1:p-1, :] += tp*(xi[1:m-1,1:n-1,1:p-1, :, 0] - xi[0:m-2,1:n-1,1:p-1, :, 0] +
                        --                                  xi[1:m-1,1:n-1,1:p-1, :, 1] - xi[1:m-1,0:n-2,1:p-1, :, 1] +
                        --          xi[1:m-1,1:n-1,1:p-1, :, 2] - xi[1:m-1,1:n-1,0:p-2, :, 2] - g[1:m-1,1:n-1,1:p-1, :])
                        then    v_el + tp*( xi_0 - xi_100_0 + xi_1 - xi_010_1 + xi_2 - xi_001_2 - g_el ) 

                    -- THE 6 FACES
                    else let one = 1 / one
                         let (i,j,k) = (i * one, j * one, k * one)
                         let one = r32(one)
                         let (v_el, g_el,xi_0,xi_1,xi_2) = (v_el*one, g_el*one,xi_0*one,xi_1*one,xi_2*one)
                         let (xi_100_0, xi_010_1, xi_001_2) = (xi_100_0*one, xi_010_1*one, xi_001_2*one)
                        in
                        if (i > 0) && (i < m-1) && (j > 0) && (j < n-1) && (k == 0) -- 1
                        -- v[1:m-1,1:n-1,0]   += tp*(xi[1:m-1,1:n-1, 0, 0] - xi[0:m-2,1:n-1, 0,0] + xi[1:m-1,1:n-1, 0,1] - 
                        --                           xi[1:m-1,0:n-2,  0,1] + xi[1:m-1,1:n-1,  0,2] - g[1:m-1,1:n-1,  0] );
                        then v_el + tp*( xi_0 - xi_100_0 + xi_1 - xi_010_1 + xi_2 - g_el ) 

                    else if (i > 0) && (i < m-1) && (j > 0) && (j < n-1) && (k == p-1) -- 2
                        -- v[1:m-1,1:n-1,p-1] += tp*(xi[1:m-1,1:n-1,p-1,0] - xi[0:m-2,1:n-1,p-1,0] + xi[1:m-1,1:n-1,p-1,1] - 
                        --                           xi[1:m-1,0:n-2,p-1,1] - xi[1:m-1,1:n-1,p-2,2] - g[1:m-1,1:n-1,p-1]);
                        then v_el + tp*( xi_0 - xi_100_0 + xi_1 - xi_010_1 - xi_001_2 - g_el ) 

                    else if (i > 0) && (i < m-1) && (j == 0) && (k > 0) && (k < p-1) -- 3
                        -- v[1:m-1,  0,1:p-1] += tp*(xi[1:m-1,  0,1:p-1,0] - xi[0:m-2,  0,1:p-1,0] + xi[1:m-1,  0,1:p-1,1] +
                        --                           xi[1:m-1,  0,1:p-1,2] - xi[1:m-1,  0,0:p-2,2] - g[1:m-1, 0,1:p-1]);
                        then v_el + tp*( xi_0 - xi_100_0 + xi_1 + xi_2 - xi_001_2 - g_el ) 

                    else if (i > 0) && (i < m-1) && (j == n-1) && (k > 0) && (k < p-1) -- 4
                        -- v[1:m-1,n-1,1:p-1] += tp*(xi[1:m-1,n-1,1:p-1,0] - xi[0:m-2,n-1,1:p-1,0] - xi[1:m-1,n-2,1:p-1,1] +
                        --                           xi[1:m-1,n-1,1:p-1,2] - xi[1:m-1,n-1,0:p-2,2] - g[1:m-1,n-1,1:p-1]);
                        then v_el + tp*( xi_0 - xi_100_0 - xi_010_1 + xi_2 - xi_001_2 - g_el )  

                    else if (i == 0) && (j > 0) && (j < n-1)&& (k > 0) && (k < p-1) -- 5
                        --  v[0,1:n-1,1:p-1] += tp*( xi[0,1:n-1,1:p-1,0] + xi[0,1:n-1,1:p-1,1] - xi[  0,0:n-2,1:p-1,1] + 
                        --                           xi[  0,1:n-1,1:p-1,2] - xi[  0,1:n-1,0:p-2,2] - g[  0,1:n-1,1:p-1]);
                        then v_el + tp*( xi_0 + xi_1 - xi_010_1 + xi_2 - xi_001_2 - g_el )    

                    else if (i == m-1) && (j > 0) && (j < n-1)&& (k > 0) && (k < p-1) -- 6
                        --  v[m-1,1:n-1,1:p-1] += tp*(-xi[m-2,1:n-1,1:p-1,0] + xi[m-1,1:n-1,1:p-1,1] - xi[m-1,0:n-2,1:p-1,1]
                        --                            + xi[m-1,1:n-1,1:p-1,2] - xi[m-1,1:n-1,0:p-2,2] - g[m-1,1:n-1,1:p-1]);
                        then v_el + tp*( -xi_100_0 + xi_1 - xi_010_1 + xi_2 - xi_001_2 - g_el )  

                    -- THE 12 EDGES
                    else if (i > 0) && (i < m-1) && (j == 0) && (k == 0) -- 1
                        --  v[1:m-1,  0,  0] += tp*(xi[1:m-1,  0,  0,0] - xi[0:m-2,  0,  0,0] + xi[1:m-1,  0,  0,1] + 
                        --                          xi[1:m-1,  0,  0,2] - g[1:m-1,  0,  0]);
                        then v_el + tp*( xi_0 - xi_100_0 + xi_1 + xi_2 - g_el )  

                    else if (i > 0) && (i < m-1) && (j == 0) && (k == p-1) -- 2
                        --  v[1:m-1,  0,p-1] += tp*(xi[1:m-1,  0,p-1,0] - xi[0:m-2,  0,p-1,0] + xi[1:m-1,  0,p-1,1] - 
                        --                          xi[1:m-1,  0,p-2,2] - g[1:m-1,  0,p-1]);
                        then v_el + tp*( xi_0 - xi_100_0 + xi_1 - xi_001_2 - g_el )  

                    else if (i > 0) && (i < m-1) && (j == n-1) && (k == 0) -- 3
                        --  v[1:m-1,n-1,  0] += tp*(xi[1:m-1,n-1,  0,0] - xi[0:m-2,n-1,  0,0] - xi[1:m-1,n-2,  0,1] + 
                        --                          xi[1:m-1,n-1,  0,2] - g[1:m-1,n-1,  0]);
                        then v_el + tp*( xi_0 - xi_100_0 - xi_010_1 + xi_2 - g_el )  

                    else if (i > 0) && (i < m-1) && (j == n-1) && (k == p-1) -- 4
                        --  v[1:m-1,n-1,p-1] += tp*(xi[1:m-1,n-1,p-1,0] - xi[0:m-2,n-1,p-1,0] - xi[1:m-1,n-2,p-1,1] - 
                        --                          xi[1:m-1,n-1,p-2,2] - g[1:m-1,n-1,p-1]);
                        then v_el + tp*( xi_0 - xi_100_0 - xi_010_1 - xi_001_2 - g_el )  

                    else if (i == 0) && (j > 0) && (j < n-1) && (k == 0) -- 5
                        --  v[  0,1:n-1,  0] += tp*( xi[  0,1:n-1,  0,0] + xi[  0,1:n-1,  0,1] - xi[  0,0:n-2,  0,1] + 
                        --                           xi[  0,1:n-1,  0,2] - g[  0,1:n-1,  0]);
                        then v_el + tp*( xi_0 + xi_1 - xi_010_1 + xi_2 - g_el )  

                    else if (i == 0) && (j > 0) && (j < n-1) && (k == p-1) -- 6
                        --  v[  0,1:n-1,p-1] += tp*( xi[  0,1:n-1,p-1,0] + xi[  0,1:n-1,p-1,1] - xi[  0,0:n-2,p-1,1] - 
                        --                           xi[  0,1:n-1,p-2,2] - g[  0,1:n-1,p-1]);
                        then v_el + tp*( xi_0 + xi_1 - xi_010_1 - xi_001_2 - g_el )  

                    else if (i == m-1) && (j > 0) && (j < n-1) && (k == 0) -- 7
                        --  v[m-1,1:n-1,  0] += tp*(-xi[m-2,1:n-1,  0,0] + xi[m-1,1:n-1,  0,1] - xi[m-1,0:n-2,  0,1] + 
                        --                           xi[m-1,1:n-1,  0,2] - g[m-1,1:n-1,  0]);
                        then v_el + tp*( -xi_100_0 + xi_1 - xi_010_1 + xi_2 - g_el )  

                    else if (i == m-1) && (j > 0) && (j < n-1) && (k == p-1) -- 8
                        --  v[m-1,1:n-1,p-1] += tp*(-xi[m-2,1:n-1,p-1,0] + xi[m-1,1:n-1,p-1,1] - xi[m-1,0:n-2,p-1,1] - 
                        --                           xi[m-1,1:n-1,p-2,2] - g[m-1,1:n-1,p-1]);
                        then v_el + tp*( -xi_100_0 + xi_1 - xi_010_1 - xi_001_2 - g_el )  

                    else if (i == 0) && (j == 0) && (k > 0) && (k < p-1) -- 9
                        --  v[  0,  0,1:p-1] += tp*( xi[  0,  0,1:p-1,0] + xi[  0,  0,1:p-1,1] + xi[  0,  0,1:p-1,2] - 
                        --                           xi[  0,  0,0:p-2,2] - g[  0,  0,1:p-1]);
                        then v_el + tp*( xi_0 + xi_1 + xi_2 - xi_001_2 - g_el )  

                    else if (i == 0) && (j == n-1) && (k > 0) && (k < p-1) -- 10
                        --  v[  0,n-1,1:p-1] += tp*( xi[  0,n-1,1:p-1,0] - xi[  0,n-2,1:p-1,1] + xi[  0,n-1,1:p-1,2] - 
                        --                           xi[  0,n-1,0:p-2,2] - g[  0,n-1,1:p-1]);
                        then v_el + tp*( xi_0 - xi_010_1 + xi_2 - xi_001_2 - g_el )  

                    else if (i == m-1) && (j == 0) && (k > 0) && (k < p-1) -- 11
                        --  v[m-1,  0,1:p-1] += tp*(-xi[m-2,  0,1:p-1,0] + xi[m-1,  0,1:p-1,1] + xi[m-1,  0,1:p-1,2] - 
                        --                           xi[m-1,  0,0:p-2,2] - g[m-1,  0,1:p-1]);
                        then v_el + tp*( -xi_100_0 + xi_1 + xi_2 - xi_001_2 - g_el )  

                    else if (i == m-1) && (j == n-1) && (k > 0) && (k < p-1) -- 12
                        --  v[m-1,n-1,1:p-1] += tp*(-xi[m-2,n-1,1:p-1,0] - xi[m-1,n-2,1:p-1,1] + xi[m-1,n-1,1:p-1,2] - 
                        --                           xi[m-1,n-1,0:p-2,2] - g[m-1,n-1,1:p-1]);
                        then v_el + tp*( -xi_100_0 - xi_010_1 + xi_2 - xi_001_2 - g_el )  

                    -- THE 8 CORNERS!
                    else if (i == 0) && (j == 0) && (k == 0) -- 1
                        --  v[0, 0, 0] += tp*( xi[0, 0, 0,0] + xi[0, 0, 0,1] + xi[0, 0, 0,2] - g[0, 0, 0])
                        then v_el + tp*( xi_0 + xi_1 + xi_2 - g_el )  

                    else if (i == 0) && (j == 0) && (k == p-1) -- 2
                        -- v[0, 0,p-1] += tp*( xi[0, 0,p-1,0] + xi[0, 0,p-1,1] - xi[0, 0,p-2,2] - g[0, 0,p-1])
                        then v_el + tp*( xi_0 + xi_1 - xi_001_2 - g_el )  

                    else if (i == 0) && (j == n-1) && (k == 0) -- 3
                        -- v[0,n-1, 0] += tp*( xi[0,n-1, 0,0] - xi[0,n-2, 0,1] + xi[0,n-1, 0,2] - g[0,n-1,0])
                        then v_el + tp*( xi_0 - xi_010_1 + xi_2 - g_el )  

                    else if (i == m-1) && (j == 0) && (k == 0) -- 4
                        -- v[m-1,0,0] += tp*(-xi[m-2,0,0,0] + xi[m-1,0,0,1] + xi[m-1,0,0,2] - g[m-1,0,0])
                        then v_el + tp*( -xi_100_0 + xi_1 + xi_2 - g_el )  

                    else if (i == 0) && (j == n-1) && (k == p-1) -- 5
                        -- v[0,n-1,p-1] += tp*( xi[0,n-1,p-1,0] - xi[0,n-2,p-1,1] - xi[0,n-1,p-2,2] - g[0,n-1,p-1])
                        then v_el + tp*( xi_0 - xi_010_1 - xi_001_2 - g_el )  

                    else if (i == m-1) && (j == 0) && (k == p-1) -- 6
                        -- v[m-1,0,p-1] += tp*(-xi[m-2,0,p-1,0] + xi[m-1,0,p-1,1] - xi[m-1,0,p-2,2] - g[m-1,0,p-1])
                        then v_el + tp*( -xi_100_0 + xi_1 - xi_001_2 - g_el )  

                    else if (i == m-1) && (j == n-1) && (k == 0) -- 7
                        -- v[m-1,n-1,0] += tp*(-xi[m-2,n-1,0,0] - xi[m-1,n-2,0,1] + xi[m-1,n-1,0,2] - g[m-1,n-1,0])
                        then v_el + tp*( -xi_100_0 - xi_010_1 + xi_2 - g_el )  

                    else if (i == m-1) && (j == n-1) && (k == p-1) -- 8
                        -- v[m-1,n-1,p-1] += tp*(-xi[m-2,n-1,p-1,0] - xi[m-1,n-2,p-1,1] - xi[m-1,n-1,p-2,2] - g[m-1,n-1,p-1])
                        then v_el + tp*( -xi_100_0 - xi_010_1 - xi_001_2 - g_el )  

                    else v_el -- redundant

     ) (iota(m*n*p*q) )
  in unflatten_4d m n p q res_flat

-----------------------------------------------------
-----------------------------------------------------
-----------------------------------------------------

let main1 [m][n][p][q] ( v:  [m][n][p][q]f32
                       , xi: [m][n][p][q](f32,f32,f32)
                       , g: [m][n][p][q]f32
                       , tp: f32 ): [m][n][p][q]f32 =

    add_descent_div3v(v, xi, g, tp)

let main(m: i32, n: i32, p: i32, q: i32, loop_count: i32): [m][n][p][q]f32 = 
    let mnpq = (m*n*p*q)
    let v  = unflatten_4d m n p q (map r32 (iota(mnpq)) )
    let g  = unflatten_4d m n p q (map r32 (iota(mnpq)) )
    let xi = unflatten_4d m n p q
                    (map (\(t: i32): (f32,f32,f32)  ->
                            let tf = 3.0f32 * r32(t) in (tf, tf+1.0f32, tf+2.0f32)
                        ) (iota(mnpq)))

    let tp = 3.0f32
    in loop(v) for _i < loop_count do add_descent_div3v(v, xi, g, tp)



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
-- add_descent_div2s implements gradient descent for the primal 
-- variable in CCP splitting algorithm. 2D scalar case. 
-- (i.e., a 2D Chan-Vese type algorithm).
----
-- Arguments:
-- v : primal variable, i.e., the label array, dimension (m,n).
-- xi: dual variable, dimensions should be (m,n,2)
-- g : data term gradient array, size (m,n).
-- tp: gradient descent time step
-----------------------------------------------------------------------
fun add_descent_div2s(v:  [m][n]f32
                 , xi: [m][n](f32,f32)
                 , g: [m][n]f32
                 , tp: f32 ): [m][n]f32 =
  map (\(i: i32): [n]f32  ->
        map (\(j: i32): f32  -> unsafe
                let v_el = v[i,j]
                let g_el = g[i,j]
                in
                if      (i > 0) && (i < m-1) && (j > 0) && (j < n-1) 
                -- INTERIOR:
                    -- v[1:m-1,1:n-1] += tp*( xi[1:m-1,1:n-1,0] - xi[0:m-2,1:n-1,0] + 
                    --                        xi[1:m-1,1:n-1,1] - xi[1:m-1,0:n-2,1] - 
                    --                        g[1:m-1,1:n-1] )
                    then let (xi_11_0, xi_11_1) = xi[i,  j]
                         let (xi_21_0, _      ) = xi[i-1,j]
                         let (_,       xi_12_1) = xi[i,j-1]
                         in v_el + tp*( xi_11_0 - xi_21_0 + xi_11_1 - xi_12_1 - g_el )
                -- THE 4 EDGES:
                else if (i > 0) && (i < m-1) && (j == 0)
                    -- v[1:m-1,0]   += tp*(xi[1:m-1, 0, 0] - xi[0:m-2, 0, 0] + xi[1:m-1, 0, 1] - g[1:m-1,  0])
                    then let (xi_10_0, xi_10_1) = xi[i,  0]
                         let (xi_20_0, _      ) = xi[i-1,0]
                         in v_el + tp*( xi_10_0 - xi_20_0 + xi_10_1 - g_el)
                else if (i > 0) && (i < m-1) && (j == n-1)
                    -- v[1:m-1,n-1] += tp*(xi[1:m-1,n-1,0] - xi[0:m-2,n-1,0] - xi[1:m-1,n-2,1] - g[1:m-1,n-1])
                    then let (xi_1, _) = xi[i,  n-1]
                         let (xi_2, _) = xi[i-1,n-1]
                         let (_, xi_3) = xi[i,  n-2]
                         in v_el + tp*( xi_1 - xi_2 - xi_3 - g_el )
                else if (i == 0) && (j > 0) && (j < n-1)
                    -- v[0,1:n-1]   += tp*( xi[0, 1:n-1, 0] + xi[0 ,1:n-1, 1] - xi[0,  0:n-2, 1] - g[0,  1:n-1])
                    then let (xi_01_0, xi_01_1) = xi[0, j  ]
                         let (_,       xi_02_1) = xi[0, j-1]
                         in v_el + tp*( xi_01_0 + xi_01_1 - xi_02_1 - g_el )
                else if (i == m-1) && (j > 0) && (j < n-1)
                    -- v[m-1,1:n-1] += tp*(-xi[m-2,1:n-1,0] + xi[m-1,1:n-1,1] - xi[m-1,0:n-2, 1] - g[m-1,1:n-1])
                    then let (xi_1, _) = xi[m-2, j  ]
                         let (_, xi_2) = xi[m-1, j  ]
                         let (_, xi_3) = xi[m-1, j-1]
                         in v_el + tp*( -xi_1 + xi_2 - xi_3 - g_el )
                -- THE FOUR CORNERS
                else if (i == 0) && (j == 0)
                    -- v[0,0]   += tp*( xi[0  ,0  ,0] + xi[0  ,0  ,1] - g[0  ,0  ])
                    then let (xi_0, xi_1) = xi[0, 0]
                         in v_el + tp*( xi_0 + xi_1 - g_el )
                else if (i == m-1) && (j == 0)
                    -- v[m-1,0] += tp*(-xi[m-2,0  ,0] + xi[m-1,0  ,1] - g[m-1,0  ])
                    then let (xi_1, _) = xi[m-2,0]
                         let (_, xi_2) = xi[m-1,0]
                         in v_el + tp*( -xi_1 + xi_2 - g_el )
                else if (i == 0) && (j == n-1)
                    -- v[0,n-1] += tp*( xi[0,n-1  ,0] - xi[0  ,n-2,1] - g[0  ,n-1])
                    then let (xi_1, _) = xi[0,n-1]
                         let (_, xi_2) = xi[0,n-2]
                         in v_el + tp*( xi_1 - xi_2 - g_el )
                else if (i == m-1) && (j == n-1)
                    -- v[-1,-1] += tp*(-xi[m-2,n-1,0] - xi[m-1,n-2,1] - g[m-1,n-1])
                    then let (xi_1, _) = xi[m-2,n-1]
                         let (_, xi_2) = xi[m-1,n-2]
                         in v_el + tp*( -xi_1 - xi_2 - g_el )
                else v_el

           ) (iota(n) )
     ) (iota(m) )


-----------------------------------------------------------------------
-- add_descent_div2v implements gradient descent for the primal 
-- variable in CCP splitting algorithm. 2D vector case. 
-- (i.e., a 2D CCP type type algorithm).
----
-- Arguments:
-- v : primal variable, i.e., the label array, dimension (m,n,k).
-- xi: dual variable, dimensions should be (m,n,k,2)
-- g : data term gradient array, size (m,n,k).
-- tp: gradient descent time step
-----------------------------------------------------------------------
fun add_descent_div2v(v:  [m][n][k]f32
                 , xi: [m][n][k](f32,f32)
                 , g: [m][n][k]f32
                 , tp: f32 ): [m][n][k]f32 =
  map (\(i: i32): [n][k]f32  ->
        map (\(j: i32): [k]f32  -> 
                map (\(q: i32): f32  -> unsafe
                        let v_el = v[i,j,q]
                        let g_el = g[i,j,q]
                        let (xi_0, xi_1) = xi[i,j,q]
                        in
                        if      (i > 0) && (i < m-1) && (j > 0) && (j < n-1) 
                        -- INTERIOR:
                            -- v[1:m-1,1:n-1, :] += tp*( xi[1:m-1,1:n-1,:,0] - xi[0:m-2,1:n-1,:,0] + 
                            --                           xi[1:m-1,1:n-1,:,1] - xi[1:m-1,0:n-2,:,1] - 
                            --                           g[1:m-1,1:n-1,:] )
                            then let (xi_21_0, _      ) = xi[i-1,j,q]
                                 let (_,       xi_12_1) = xi[i,j-1,q]
                                 in v_el + tp*( xi_0 - xi_21_0 + xi_1 - xi_12_1 - g_el )
                        -- THE 4 EDGES:
                        else if (i > 0) && (i < m-1) && (j == 0)
                            -- v[1:m-1,0,:] += tp*( xi[1:m-1, 0, :, 0] - xi[0:m-2, 0, :, 0] + 
                            --                      xi[1:m-1, 0, :, 1] - g[1:m-1,  0 ,:])
                            then let (xi_20_0, _      ) = xi[i-1,0,q]
                                 in v_el + tp*( xi_0 - xi_20_0 + xi_1 - g_el)
                        else if (i > 0) && (i < m-1) && (j == n-1)
                            -- v[1:m-1,n-1,:] += tp*( xi[1:m-1,n-1,:, 0] - xi[0:m-2,n-1,:, 0] - 
                            --                        xi[1:m-1,n-2,:, 1] - g[1:m-1,n-1 ,:] )
                            then let (xi_2, _) = xi[i-1,n-1,q]
                                 let (_, xi_3) = xi[i,  n-2,q]
                                 in v_el + tp*( xi_1 - xi_2 - xi_3 - g_el )
                        else if (i == 0) && (j > 0) && (j < n-1)
                            -- v[0,1:n-1,:]   += tp*( xi[0, 1:n-1,:,  0] + xi[0 ,1:n-1, :, 1] - 
                            --                        xi[0,  0:n-2, :, 1] - g[0,  1:n-1 ,:] )
                            then let (_,       xi_02_1) = xi[0,j-1,q]
                                 in v_el + tp*( xi_0 + xi_1 - xi_02_1 - g_el )
                        else if (i == m-1) && (j > 0) && (j < n-1)
                            -- v[m-1,1:n-1,:] += tp*( -xi[m-2,1:n-1,:, 0] + xi[m-1,1:n-1,:, 1] - 
                            --                        xi[m-1,0:n-2, :, 1] - g[m-1,1:n-1 ,:] )
                            then let (xi_2, _) = xi[m-2,j,  q]
                                 let (_, xi_3) = xi[m-1,j-1,q]
                                 in v_el + tp*( -xi_2 + xi_1 - xi_3 - g_el )
                        -- THE FOUR CORNERS
                        else if (i == 0) && (j == 0)
                            -- v[0,0,:]   += tp*( xi[0  ,0  ,:, 0] + xi[0  ,0  ,:, 1] - g[0  ,0  ,:])
                            then v_el + tp*( xi_0 + xi_1 - g_el )
                        else if (i == m-1) && (j == 0)
                            -- v[m-1,0,:] += tp*(-xi[m-2,0  ,:, 0] + xi[m-1,0  ,:, 1] - g[m-1,0  ,:])
                            then let (xi_2, _) = xi[m-2,0,q]
                                 in v_el + tp*( -xi_2 + xi_1 - g_el )
                        else if (i == 0) && (j == n-1)
                            -- v[0,n-1,:] += tp*( xi[0,n-1  ,:, 0] - xi[0  ,n-2,:, 1] - g[0  ,n-1,:])
                            then let (_, xi_2) = xi[0,n-2,q]
                                 in v_el + tp*( xi_0 - xi_2 - g_el )
                        else if (i == m-1) && (j == n-1)
                            -- v[-1,-1,:] += tp*(-xi[m-2,n-1,:, 0] - xi[m-1,n-2,:, 1] - g[m-1,n-1,:])
                            then let (xi_2, _) = xi[m-2,n-1,q]
                                 let (_, xi_3) = xi[m-1,n-2,q]
                                 in v_el + tp*( -xi_2 - xi_3 - g_el )
                        else v_el

                   ) (iota(k) )
           ) (iota(n) )
     ) (iota(m) )

-----------------------------------------------------
-----------------------------------------------------
-----------------------------------------------------

fun main(v:  [m][n]f32
                    , xi_0: [m][n]f32
                    , xi_1: [m][n]f32
                    , g: [m][n]f32
                    , tp: f32 ): [m][n]f32 =
    add_descent_div2s(v, zip@1 xi_0 xi_1, g, tp)

fun main2(v:  [m][n][k]f32
                        , xi: [m][n][k](f32,f32)
                        , g: [m][n][k]f32
                        , tp: f32 ): [m][n][k]f32 =
    add_descent_div2v(v, xi, g, tp)

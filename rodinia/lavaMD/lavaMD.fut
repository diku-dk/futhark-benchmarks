-- Code and comments based on
-- https://github.com/kkushagra/rodinia/blob/master/openmp/lavaMD/main.c
--
-- ==
-- compiled input @ data/3_boxes.in
-- output @ data/3_boxes.out
-- notravis input @ data/10_boxes.in
-- output @ data/10_boxes.out

default(f32)

fun int num_neighbors()      = 27
fun int number_par_per_box() = 100 

fun f32 dot((f32,f32,f32) a, (f32,f32,f32) b) =
    let (ax,ay,az) = a in
    let (bx,by,bz) = b in
    ax*bx + ay*by + az*bz

-----------------------------------------
-- Main Computational Kernel of lavaMD --
-----------------------------------------
fun  [number_boxes][par_per_box](f32,f32,f32,f32)  -- result fv
  main( f32 alpha
            , [number_boxes](int,int,int,int)       box_coefs
            , [][number_boxes](int,int,int,int)     box_nnghs  -- outer dim should be num_neighbors
            , [number_boxes]int                     box_num_nghbs
            , [number_boxes][par_per_box](f32,f32,f32,f32) rv
            , [number_boxes][par_per_box]f32 qv
            ) =
  let a2 = 2.0*alpha*alpha in
  map( fn [par_per_box](f32,f32,f32,f32) (int l) =>
        let ( bl_x, bl_y, bl_bz, bl_number ) = box_coefs[l] in
        let rA = rv[l] in
        map (fn (f32,f32,f32,f32) ( (f32,f32,f32,f32) rA_el ) => --(int i) =>
                let (rai_v, rai_x, rai_y, rai_z) = rA_el in -- rA[i] in
                let acc = (0.0,0.0,0.0,0.0) in
                loop(acc) = 
                  for k < box_num_nghbs[l]+1 do
                          let pointer = if (k > 0)
                                        then let (_,_,_,num) = unsafe box_nnghs[k-1, l] in num
                                        else l
                          in
                          let (_,_,_,first_j) = unsafe box_coefs[pointer] in
                          let rB = unsafe rv[first_j] in
                          let qB = unsafe qv[first_j] in
                          ---------------------------------------------------------
                          -- Important note: rB and qB are invariant to the      --
                          -- second map on rA => can be blocked in shared memory --
                          ---------------------------------------------------------
                          let pres = 
                            map( fn (f32,f32,f32,f32) ( ( (f32,f32,f32,f32), f32 ) tup) =>
                                      let ( (rbj_v,rbj_x,rbj_y,rbj_z), qbj ) = tup in
                                      let r2   = rai_v + rbj_v - dot((rai_x,rai_y,rai_z), (rbj_x,rbj_y,rbj_z)) in
                                      let u2   = a2*r2          in
                                      let vij  = exp32(-u2)       in
                                      let fs   = 2.0 * vij      in
                                      let d_x  = rai_x  - rbj_x in 
                                      let d_y  = rai_y  - rbj_y in 
                                      let d_z  = rai_z  - rbj_z in 
                                      let fxij = fs * d_x       in
                                      let fyij = fs * d_y       in
                                      let fzij = fs * d_z       in
                                      (qbj*vij, qbj*fxij, qbj*fyij, qbj*fzij)
                               , zip(rB,qB) )

                          let (r1, r2, r3, r4) = 
                            reduce( fn (f32,f32,f32,f32) ((f32,f32,f32,f32) a, (f32,f32,f32,f32) b) =>
                                        let (a1,a2,a3,a4) = a in let (b1,b2,b3,b4) = b in (a1+b1, a2+b2, a3+b3, a4+b4)
                                  , (0.0,0.0,0.0,0.0), pres)
                          let (a1, a2, a3, a4) = acc
                          in  (a1+r1, a2+r2, a3+r3, a4+r4)
                in acc                         
            , rA )  -- iota(par_per_box) )
     , iota(number_boxes) )

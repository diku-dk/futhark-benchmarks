-- Code and comments based on
-- https://github.com/kkushagra/rodinia/blob/master/openmp/lavaMD/main.c
--
-- ==
-- input @ data/3_boxes.in
-- output @ data/3_boxes.out
-- compiled input @ data/10_boxes.in.gz
-- output @ data/10_boxes.out.gz

def dot (ax,ay,az) (bx,by,bz): f32 =
  ax*bx + ay*by + az*bz

-----------------------------------------
-- Main Computational Kernel of lavaMD --
-----------------------------------------
def main [number_boxes][par_per_box][num_neighbors]
             (alpha:  f32)
             (box_coefs_0: [number_boxes]i32)
             (box_coefs_1: [number_boxes]i32)
             (box_coefs_2: [number_boxes]i32)
             (box_coefs_3: [number_boxes]i32)
             (box_nnghs_0 : [num_neighbors][number_boxes]i32)
             (box_nnghs_1 : [num_neighbors][number_boxes]i32)
             (box_nnghs_2 : [num_neighbors][number_boxes]i32)
             (box_nnghs_3 : [num_neighbors][number_boxes]i32)
             (box_num_nghbs: [number_boxes]i32)
             (rv_0: [number_boxes][par_per_box]f32)
             (rv_1: [number_boxes][par_per_box]f32)
             (rv_2: [number_boxes][par_per_box]f32)
             (rv_3: [number_boxes][par_per_box]f32)
             (qv: [number_boxes][par_per_box]f32)
      : ([number_boxes][par_per_box]f32,
         [number_boxes][par_per_box]f32,
         [number_boxes][par_per_box]f32,
         [number_boxes][par_per_box]f32) =
  let box_coefs = zip4 (box_coefs_0) (box_coefs_1) (box_coefs_2) (box_coefs_3)
  let box_nnghs = map4 zip4 box_nnghs_0 box_nnghs_1 box_nnghs_2 box_nnghs_3
  let rv = map4 zip4 rv_0 rv_1 rv_2 rv_3
  let a2 = 2.0*alpha*alpha in
  unzip4 <| map unzip4 (map2 (\box_num_nghbs' (l: i64): [par_per_box](f32,f32,f32,f32)  ->
        let rA = rv[l]
        in
        map  (\(rA_el:  (f32,f32,f32,f32) ): (f32,f32,f32,f32)  -> --(i32 i) ->
                let (rai_v, rai_x, rai_y, rai_z) = rA_el in -- rA[i]
                let acc = (0.0,0.0,0.0,0.0)
                in loop(acc) for k < box_num_nghbs'+1 do
                          let pointer = if (k > 0)
                                        then let (_,_,_,num) = #[unsafe] box_nnghs[k-1, l] in num
                                        else i32.i64 l

                          let (_,_,_,first_j) = #[unsafe] box_coefs[pointer]
                          let rB = #[unsafe] rv[first_j]
                          let qB = #[unsafe] qv[first_j]
                          let (rbj_v,rbj_x,rbj_y,rbj_z) = unzip4 rB
                          let qbj = qB
                          let r2   = rai_v + rbj_v - dot (zip3 rai_x rai_y rai_z) (zip3 rbj_x rbj_y rbj_z)
                          let u2   = a2*r2
                          let vij  = f32.exp(0-u2)
                          let fs   = 2.0 * vij
                          let d_x  = rai_x  - rbj_x
                          let d_y  = rai_y  - rbj_y
                          let d_z  = rai_z  - rbj_z
                          let fxij = fs * d_x
                          let fyij = fs * d_y
                          let fzij = fs * d_z
                          let (r1, r2, r3, r4) =
                            (f32.sum (qbj*vij),
                             f32.sum (qbj*fxij),
                             f32.sum (qbj*fyij),
                             f32.sum (qbj*fzij))
                          let (a1, a2, a3, a4) = acc
                          in  (a1+r1, a2+r2, a3+r3, a4+r4)
            ) rA
     ) box_num_nghbs (iota(number_boxes)))

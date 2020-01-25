-- Code and comments based on
-- https://github.com/kkushagra/rodinia/blob/master/openmp/nw
--
-- ==
--
-- compiled input @ data/large.in.gz
-- output @ data/large.out.gz

-- compiled input @ data/tiny.in
-- output @ data/tiny.out
-- compiled input @ data/small.in
-- output @ data/small.out
-- compiled input @ data/medium.in.gz
-- output @ data/medium.out.gz

type int = i32

let B:int = 32

let fInd (y:i32) (x:i32): i32 = y*(B+1) + x
let max3 (x:int, y:int, z:int) = if x < y 
                                 then if y < z then z else y
                                 else if x < z then z else x
let mkVal [l2][l] (y:i32) (x:i32) (pen:int) (inp_l:[l2]int) (ref_l:[l][l]int) : int = unsafe
  max3( ( (inp_l[fInd (y-1) (x-1)])) + ( ref_l[y-1, x-1])
      , ( (inp_l[fInd y (x-1)])) - pen
      , ( (inp_l[fInd (y-1) x])) - pen
      )
    
let intraBlockPar [lensq][len] (penalty: int)
                               (inputsets: [lensq]int) 
                               (reference2: [len][len]int) 
                               (b_y: i32) (b_x: i32)
                               : [B][B]int =
  -- index   =   base + cols * BLOCK_SIZE * b_index_y + BLOCK_SIZE * b_index_x + tx + ( cols + 1 );
  -- for ( int ty = 0 ; ty < BLOCK_SIZE ; ty++)
  --   REF(ty, tx) =  reference_d[index + cols * ty];
  let slice_ref =  unsafe reference2[b_y*B+1 : b_y*B+1+B, b_x*B+1 : b_x*B+1+B] :> [B][B]int

  let ref_l = replicate (B*B) 0
  let ref_l = loop ref_l for i < B do
                scatter ref_l (map (\tid->i*B+tid) (iota B)) slice_ref[i]
  let ref_l = unflatten B B ref_l

  let inp_l = replicate ((B+1)*(B+1)) 0i32

  -- index_nw =  base + cols * BLOCK_SIZE * b_index_y + BLOCK_SIZE * b_index_x;
  let index_nw =  len * B * b_y + B * b_x                
  let inp_l[0] = unsafe inputsets[index_nw]

  --index_w   = base + cols * BLOCK_SIZE * b_index_y + BLOCK_SIZE * b_index_x + ( cols );
  let index_w = len*B*b_y + B*b_x + len 
  -- SCORE((tx + 1), 0) = input_itemsets_d[index_w + cols * tx];
  let inp_l = scatter inp_l (map (\tx->(tx+1)*(B+1)) (iota B))
                      (map (\tx->unsafe inputsets[index_w+len*tx]) (iota B))

  --index_n   = base + cols * BLOCK_SIZE * b_index_y + BLOCK_SIZE * b_index_x + tx + ( 1 );
  let index_n = len*B*b_y + B*b_x + 1
  -- SCORE(0, (tx + 1)) = input_itemsets_d[index_n];
  let inp_l = scatter inp_l (map (+1) (iota B))
                      (map (\tx->unsafe inputsets[index_n+tx]) (iota B))

  let inp_l = loop inp_l for m < B do
        let (inds, vals) = unzip (
            map (\tx ->  (
                    if tx > m then (-1, 0)
                    else let ind_x = tx + 1
                         let ind_y = m - tx + 1
                         let v = mkVal ind_y ind_x penalty inp_l ref_l
                         in  (fInd ind_y ind_x, v) )
                ) (iota B) )
        in  scatter inp_l inds vals

  let inp_l = loop inp_l for m < B-1 do
        let m = B - 2 - m
        let (inds, vals) = unzip (
            map (\tx ->  (
                    if tx > m then (-1, 0)
                    else let ind_x = tx + B - m
                         let ind_y = B - tx
                         let v = mkVal ind_y ind_x penalty inp_l ref_l
                         in  (fInd ind_y ind_x, v) )
                ) (iota B) )
        in  scatter inp_l inds vals

  let inp_l2 = unflatten (B+1) (B+1) inp_l
  in  inp_l2[1:B+1,1:B+1] :> [B][B]int


let updateBlocks [q][lensq] (len: i32) (blk: i32)
                            (mk_b_y: (i32 -> i32)) 
                            (mk_b_x: (i32 -> i32)) 
                            (block_inp: [q][B][B]int) 
                            (inputsets:  *[lensq]int) =
  let (inds, vals) = unzip (
    map (\gid -> let B2 = B*B
                 let (bx, lid2) = (gid / B2, gid % B2)
                 let (ty, tx)   = (lid2 / B, lid2 % B)

                 let b_y = mk_b_y bx
                 let b_x = mk_b_x bx
                 let v = unsafe block_inp[bx, ty, tx]    
                 let ind = (B*b_y + 1 + ty) * len + (B*b_x + tx + 1)
                 in  (ind, v)
        ) (iota (blk*B*B)) )
  in  scatter inputsets inds vals


-- `len-1` should be a multiple of 16 
let main [lensq] (penalty : int) 
                 (inputsets : *[lensq]int) 
                 (reference : *[lensq]int) : [lensq]int =
  let len = t32 (f32.sqrt (r32 lensq))
  let worksize = len - 1
  let block_width = worksize / B
  let reference2 = unflatten len len reference

  -- First loop BEGINS
  let inputsets =
    loop inputsets for blk < block_width do
        let blk = blk + 1
        let block_inp =
          map (\b_x ->
                let b_y = blk-1-b_x
                in  intraBlockPar penalty inputsets reference2 b_y b_x
          ) (iota blk)

        let mkBY bx = blk - 1 - bx
        let mkBX bx = bx
        in  updateBlocks len blk mkBY mkBX block_inp inputsets
  -- First loop ENDS

  -- Second loop BEGINS
  let inputsets =
    loop inputsets for blk < block_width-1 do
        let blk = block_width - 1 - blk
        let block_inp =
          map (\bx ->
                let b_y = block_width - 1 - bx
                let b_x = bx + block_width - blk
                in  intraBlockPar penalty inputsets reference2 b_y b_x
          ) (iota blk)

        let mkBY bx = block_width - 1 - bx
        let mkBX bx = bx + block_width - blk
        in  updateBlocks len blk mkBY mkBX block_inp inputsets
    -- Second loop ENDS

  in  inputsets

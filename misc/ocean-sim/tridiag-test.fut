import "tridiag"
import "typeinst"

let INNER_DIM : i64 = 115

-- ==
-- entry: tridagNested
--
-- compiled random input { [57600][115]f32 [57600][115]f32 [57600][115]f32 [57600][115]f32 }
entry tridagNested [n][m] (a: [n][m]DTYPE) (b: [n][m]DTYPE) (c: [n][m]DTYPE) (y: [n][m]DTYPE): *[n][m]DTYPE =
   map4 (\a b c y -> tridagPar (a,b,c,y)) a b c y

-- ==
-- entry: tridagNestedConst
--
-- compiled random input { [57600][115]f32 [57600][115]f32 [57600][115]f32 [57600][115]f32 }
entry tridagNestedConst [n] (a: [n][INNER_DIM]DTYPE) (b: [n][INNER_DIM]DTYPE) (c: [n][INNER_DIM]DTYPE) (y: [n][INNER_DIM]DTYPE): *[n][INNER_DIM]DTYPE =
   map4 (\a b c y -> tridagPar (a,b,c,y)) a b c y

   -- ==
-- entry: tridagNestedSeqConst
--
-- compiled random input { [57600][115]f32 [57600][115]f32 [57600][115]f32 [57600][115]f32 }
entry tridagNestedSeqConst [n] (a: [n][INNER_DIM]DTYPE) (b: [n][INNER_DIM]DTYPE) (c: [n][INNER_DIM]DTYPE) (y: [n][INNER_DIM]DTYPE): *[n][INNER_DIM]DTYPE =
   #[sequential_inner]
   map4 (\a b c y -> tridagSeq (a,b,c,y)) a b c y

-- ==
-- entry: tridagNestedSeq
--
-- compiled random input { [57600][115]f32 [57600][115]f32 [57600][115]f32 [57600][115]f32 }
entry tridagNestedSeq [n][m] (a: [n][m]DTYPE) (b: [n][m]DTYPE) (c: [n][m]DTYPE) (y: [n][m]DTYPE): *[n][m]DTYPE =
   #[sequential_inner]
   map4 (\a b c y -> tridagSeq (a,b,c,y)) a b c y
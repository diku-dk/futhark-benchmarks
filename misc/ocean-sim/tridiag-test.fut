import "typeinst32"
import "tridiag"

def INNER_DIM : i64 = 115

-- ==
-- entry: tridagNested tridagNestedConst tridagNestedSeq tridagNestedSeqConst 
--
-- compiled input @ data/tridiag32-small.in
-- output @ data/tridiag32-small.out
--
-- compiled random input { [57600][115]f32 [57600][115]f32 [57600][115]f32 [57600][115]f32 }


entry tridagNested [n][m] (a: [n][m]DTYPE) (b: [n][m]DTYPE) (c: [n][m]DTYPE) (y: [n][m]DTYPE): *[n][m]DTYPE =
   map4 (\a b c y -> tridagPar (a,b,c,y)) a b c y

entry tridagNestedConst [n] (a: [n][INNER_DIM]DTYPE) (b: [n][INNER_DIM]DTYPE) (c: [n][INNER_DIM]DTYPE) (y: [n][INNER_DIM]DTYPE): *[n][INNER_DIM]DTYPE =
   map4 (\a b c y -> tridagPar (a,b,c,y)) a b c y

entry tridagNestedSeq [n][m] (a: [n][m]DTYPE) (b: [n][m]DTYPE) (c: [n][m]DTYPE) (y: [n][m]DTYPE): *[n][m]DTYPE =
   #[sequential_inner]
   map4 (\a b c y -> tridagSeq (a,b,c,y)) a b c y

entry tridagNestedSeqConst [n] (a: [n][INNER_DIM]DTYPE) (b: [n][INNER_DIM]DTYPE) (c: [n][INNER_DIM]DTYPE) (y: [n][INNER_DIM]DTYPE): *[n][INNER_DIM]DTYPE =
   #[sequential_inner]
   map4 (\a b c y -> tridagSeq (a,b,c,y)) a b c y
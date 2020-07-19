module P = import "poseidon"

-- ==
-- entry: arity8
-- compiled random input { [700000]u64 }
-- auto output

let x8 = P.p8.init P.p8.blank_constants
entry arity8 input = (P.mbatch_hash8 x8 input).0

-- ==
-- entry: arity11
-- compiled random input { [700040]u64 }
-- auto output

let x11 = P.p11.init P.p11.blank_constants
entry arity11 input = (P.mbatch_hash11 x11 input).0

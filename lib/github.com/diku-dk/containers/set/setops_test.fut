-- | ignore

import "setops"

-- ==
-- entry: test_intersect
-- input { [2i64,4i64,3i64] [4i64,6i64] } output { [4i64] }
-- input { [2i64,4i64,3i64] empty([0]i64) } output { empty([0]i64) }
-- input { empty([0]i64) [4i64,6i64] } output { empty([0]i64) }
entry test_intersect (a: []i64) (b: []i64) : []i64 =
  setops.intersect a b

-- ==
-- entry: test_union
-- input { [2i64,4i64,3i64] [4i64,6i64] } output { [2i64,3i64,4i64,6i64] }
-- input { [2i64,4i64,3i64] empty([0]i64) } output { [2i64,3i64,4i64] }
-- input { empty([0]i64) [4i64,6i64] } output { [4i64,6i64] }
entry test_union (a: []i64) (b: []i64) : []i64 =
  setops.union a b

-- ==
-- entry: test_diff
-- input { [2i64,4i64,3i64] [4i64,6i64] } output { [2i64,3i64] }
-- input { [2i64,4i64,3i64] empty([0]i64) } output { [2i64,3i64,4i64] }
-- input { empty([0]i64) [4i64,6i64] } output { empty([0]i64) }
entry test_diff (a: []i64) (b: []i64) : []i64 =
  setops.diff a b

-- ==
-- entry: test_elimdups
-- input { [2i64,4i64,3i64,4i64] } output { [2i64,3i64,4i64] }
-- input { [2i64,4i64,2i64] } output { [2i64,4i64] }
-- input { empty([0]i64) } output { empty([0]i64) }
entry test_elimdups (a: []i64) : []i64 =
  setops.elimdups a

-- | ignore

import "bitset"

module bitset_u8 = mk_bitset u8

def capacity = 1 + i64.u8 u8.highest

-- ==
-- entry: test_is_empty_success
-- random input { [100]u8 } output { true }
entry test_is_empty_success (arr: []u8) : bool =
  all (\c ->
         let c' = i64.u8 c
         let empty_set = bitset_u8.empty c'
         in bitset_u8.is_empty empty_set)
      arr

-- ==
-- entry: test_is_empty_fail
-- random input { [100][20]u8 } output { false }
entry test_is_empty_fail (arr: [][]u8) : bool =
  all (\arr' ->
         let arr_i64 = map i64.u8 arr'
         let set = bitset_u8.from_array capacity arr_i64
         in bitset_u8.is_empty set)
      arr

-- ==
-- entry: test_complement
-- input { 0u8 } output { 0i64 }
-- input { 1u8 } output { 1i64 }
-- input { 2u8 } output { 2i64 }
-- input { 3u8 } output { 3i64 }
-- input { 4u8 } output { 4i64 }
-- input { 5u8 } output { 5i64 }
-- input { 6u8 } output { 6i64 }
-- input { 7u8 } output { 7i64 }
-- input { 8u8 } output { 8i64 }
-- input { 9u8 } output { 9i64 }
-- input { 10u8 } output { 10i64 }
-- input { 11u8 } output { 11i64 }
-- input { 12u8 } output { 12i64 }
-- input { 13u8 } output { 13i64 }
entry test_complement (c: u8) : i64 =
  let c' = i64.u8 c
  let empty_set = bitset_u8.empty c'
  let full_set = bitset_u8.complement empty_set
  let result = bitset_u8.size full_set
  in result

-- ==
-- entry: test_equality
-- random input { [100][20]u8 } output { true }
entry test_equality (arr: [][]u8) : bool =
  all (\arr ->
         let arr' = map i64.u8 arr
         let set = bitset_u8.from_array capacity arr'
         in set bitset_u8.== set)
      arr

-- ==
-- entry: test_singleton
-- random input { [100]u8 } output { true }
entry test_singleton (arr: []u8) : bool =
  all (\i ->
         let singleton = bitset_u8.singleton capacity (i64.u8 i)
         in bitset_u8.size singleton == 1)
      arr

-- ==
-- entry: test_delete_success
-- random input { [100]u8 } output { true }
entry test_delete_success (arr: []u8) : bool =
  all (\i ->
         let i' = i64.u8 i
         let singleton = bitset_u8.singleton capacity i'
         let empty_set = bitset_u8.delete (copy singleton) [i']
         in bitset_u8.size singleton == 1 && bitset_u8.size empty_set == 0)
      arr

-- ==
-- entry: test_delete_fail
-- random input { [100]u8 } output { true }
entry test_delete_fail (arr: []u8) : bool =
  all (\i ->
         let i' = i64.u8 i
         let arr' = filter (!= i') (0...capacity)
         let singleton = bitset_u8.singleton capacity i'
         let not_empty_sets = map (\j -> bitset_u8.delete (copy singleton) [j]) arr'
         in bitset_u8.size singleton == 1 && all ((== 1) <-< bitset_u8.size) not_empty_sets)
      arr

-- ==
-- entry: test_member_success
-- random input { [100][20]u8 } output { true }
entry test_member_success (arr: [][]u8) : bool =
  all (\arr' ->
         let arr_i64 = map i64.u8 arr'
         let set = bitset_u8.from_array capacity arr_i64
         in all (flip bitset_u8.member set) arr_i64)
      arr

-- ==
-- entry: test_member_fail
-- random input { [100][20]u8 } output { false }
entry test_member_fail (arr: [][]u8) : bool =
  all (\arr' ->
         let arr_i64 = map ((+ 256) <-< i64.u8) arr'
         let set = bitset_u8.from_array capacity arr_i64
         in all (flip bitset_u8.member set) arr_i64)
      arr

-- ==
-- entry: test_insert
-- random input { [100]u8 } output { true }
entry test_insert (arr: []u8) : bool =
  all (\i ->
         let i' = i64.u8 i
         let arr' = filter (!= i') (0...capacity)
         let set = bitset_u8.empty capacity
         let new_set = bitset_u8.insert (copy set) [i']
         in bitset_u8.size set == 0
            && bitset_u8.size new_set == 1
            && bitset_u8.member i' new_set
            && all (not <-< flip bitset_u8.member new_set) arr'
            && bitset_u8.member i' set
            |> not)
      arr

-- ==
-- entry: test_union
-- input { empty([0]i64) empty([0]i64) empty([0]i64) } output { true }
-- input { [0i64] [0i64] [0i64] } output { true }
-- input { [0i64] [1i64] [0i64, 1i64] } output { true }
-- input { [0i64, 0i64] [1i64] [0i64, 1i64] } output { true }
-- input { [0i64, 0i64] [1i64] [0i64, 1i64, 2i64] } output { false }
entry test_union (a: []i64) (b: []i64) (c: []i64) : bool =
  let a_set = bitset_u8.from_array capacity a
  let b_set = bitset_u8.from_array capacity b
  let c_set = bitset_u8.from_array capacity c
  in bitset_u8.union a_set b_set bitset_u8.== c_set

-- ==
-- entry: test_intersection
-- input { empty([0]i64) empty([0]i64) empty([0]i64) } output { true }
-- input { [0i64] [0i64] [0i64] } output { true }
-- input { [0i64] [1i64] empty([0]i64) } output { true }
-- input { [0i64, 0i64] [1i64] empty([0]i64) } output { true }
-- input { [0i64, 0i64] [1i64] [2i64] } output { false }
entry test_intersection (a: []i64) (b: []i64) (c: []i64) : bool =
  let a_set = bitset_u8.from_array capacity a
  let b_set = bitset_u8.from_array capacity b
  let c_set = bitset_u8.from_array capacity c
  in bitset_u8.intersection a_set b_set bitset_u8.== c_set

-- ==
-- entry: test_difference
-- input { empty([0]i64) empty([0]i64) empty([0]i64) } output { true }
-- input { [0i64] [0i64] empty([0]i64)  } output { true }
-- input { [0i64] [1i64] [0i64] } output { true }
-- input { [0i64, 0i64] [1i64] [0i64] } output { true }
-- input { [0i64, 0i64] [1i64] [2i64] } output { false }
entry test_difference (a: []i64) (b: []i64) (c: []i64) : bool =
  let a_set = bitset_u8.from_array capacity a
  let b_set = bitset_u8.from_array capacity b
  let c_set = bitset_u8.from_array capacity c
  in bitset_u8.difference a_set b_set bitset_u8.== c_set

-- ==
-- entry: test_is_subset
-- input { empty([0]i64) empty([0]i64) } output { true }
-- input { [0i64] [0i64] } output { true }
-- input { [1i64] [0i64, 1i64] } output { true }
-- input { [0i64, 0i64] [1i64] } output { false }
-- input { [0i64, 3i64] [2i64] } output { false }
entry test_is_subset (a: []i64) (b: []i64) : bool =
  let a_set = bitset_u8.from_array capacity a
  let b_set = bitset_u8.from_array capacity b
  in bitset_u8.is_subset a_set b_set

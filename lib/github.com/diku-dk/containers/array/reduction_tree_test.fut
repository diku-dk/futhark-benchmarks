import "reduction_tree"

def backwards_linear_search [n] 't
                            (op: t -> t -> bool)
                            (arr: [n]t)
                            (i: i64) : i64 =
  if i < 0 || n <= i
  then -1
  else loop j = i - 1
       while j != -1 && not (arr[j] `op` arr[i]) do
         j - 1

def linear_search [n] 't
                  (op: t -> t -> bool)
                  (arr: [n]t)
                  (i: i64) : i64 =
  if i < 0 || n <= i
  then -1
  else let k =
         loop j = i + 1
         while j != n && not (arr[j] `op` arr[i]) do
           j + 1
       in if k == n then -1 else k

module mintree = mk_mintree i64

-- ==
-- entry: test_mintree_previous
-- nobench compiled random input { [10000]i64 }
-- output { true }
-- nobench compiled random input { [1024]i64 }
-- output { true }
entry test_mintree_previous [n] (arr: [n]i64) =
  let tree = mintree.make arr
  let expected = tabulate n (backwards_linear_search (<=) arr)
  let result = tabulate n (mintree.previous tree)
  in zip expected result
     |> all (uncurry (==))

-- ==
-- entry: test_mintree_next
-- nobench compiled random input { [10000]i64 }
-- output { true }
-- nobench compiled random input { [1024]i64 }
-- output { true }
entry test_mintree_next [n] (arr: [n]i64) =
  let tree = mintree.make arr
  let expected = tabulate n (linear_search (<=) arr)
  let result = tabulate n (mintree.next tree)
  in zip expected result
     |> all (uncurry (==))

-- ==
-- entry: test_mintree_strict_previous
-- nobench compiled random input { [10000]i64 }
-- output { true }
-- nobench compiled random input { [1024]i64 }
-- output { true }
entry test_mintree_strict_previous [n] (arr: [n]i64) =
  let tree = mintree.make arr
  let expected = tabulate n (backwards_linear_search (<) arr)
  let result = tabulate n (mintree.strict_previous tree)
  in zip expected result
     |> all (uncurry (==))

-- ==
-- entry: test_mintree_strict_next
-- nobench compiled random input { [10000]i64 }
-- output { true }
-- nobench compiled random input { [1024]i64 }
-- output { true }
entry test_mintree_strict_next [n] (arr: [n]i64) =
  let tree = mintree.make arr
  let expected = tabulate n (linear_search (<) arr)
  let result = tabulate n (mintree.strict_next tree)
  in zip expected result
     |> all (uncurry (==))

module maxtree = mk_maxtree i64

-- ==
-- entry: test_mintree_previous
-- nobench compiled random input { [10000]i64 }
-- output { true }
-- nobench compiled random input { [1024]i64 }
-- output { true }
entry test_maxtree_previous [n] (arr: [n]i64) =
  let tree = maxtree.make arr
  let expected = tabulate n (backwards_linear_search (>=) arr)
  let result = tabulate n (maxtree.previous tree)
  in zip expected result
     |> all (uncurry (==))

-- ==
-- entry: test_maxtree_next
-- nobench compiled random input { [10000]i64 }
-- output { true }
-- nobench compiled random input { [1024]i64 }
-- output { true }
entry test_maxtree_next [n] (arr: [n]i64) =
  let tree = maxtree.make arr
  let expected = tabulate n (linear_search (>=) arr)
  let result = tabulate n (maxtree.next tree)
  in zip expected result
     |> all (uncurry (==))

-- ==
-- entry: test_maxtree_strict_previous
-- nobench compiled random input { [10000]i64 }
-- output { true }
-- nobench compiled random input { [1024]i64 }
-- output { true }
entry test_maxtree_strict_previous [n] (arr: [n]i64) =
  let tree = maxtree.make arr
  let expected = tabulate n (backwards_linear_search (>) arr)
  let result = tabulate n (maxtree.strict_previous tree)
  in zip expected result
     |> all (uncurry (==))

-- ==
-- entry: test_maxtree_strict_next
-- nobench compiled random input { [10000]i64 }
-- output { true }
-- nobench compiled random input { [1024]i64 }
-- output { true }
entry test_maxtree_strict_next [n] (arr: [n]i64) =
  let tree = maxtree.make arr
  let expected = tabulate n (linear_search (>) arr)
  let result = tabulate n (maxtree.strict_next tree)
  in zip expected result
     |> all (uncurry (==))

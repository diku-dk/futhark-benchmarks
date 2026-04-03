-- | Set-like operations on arrays of values interpreted as sets

import "../sorts/radix_sort"
import "opt"

local
-- | The setops module type.  This module type is declared `local`, which means
-- that it may not be referenced directly by name from client code.  This
-- limitation makes it possible for the interface to be enriched by new members
-- in future minor versions.
module type setops = {
  -- | `union a b` returns the set of elements that appear in both `a` and `b`
  -- with duplicates removed. Work: O(`n`+`m`), span: O(1).
  val union [m] [n] : [m]i64 -> [n]i64 -> ?[k].[k]i64

  -- | `intersect a b` returns those elements in `a` that also appears in `b` with
  -- duplicates removed. Work: O(`n`+`m`), span: O(1).
  val intersect [m] [n] : [m]i64 -> [n]i64 -> ?[k].[k]i64

  -- | `diff a b` returns `a` with duplicates and elements from `b`
  -- removed. Work: O(`n`+`m`), span: O(1).
  val diff [m] [n] : [m]i64 -> [n]i64 -> ?[k].[k]i64

  -- | `elimdups a` returns `a` with duplicates removed. Work: O(`n`), span:
  -- O(1).
  val elimdups [m] : [m]i64 -> ?[k].[k]i64

  -- | `union_by_key key f a b` returns the set of elements that appear in both
  -- `a` and `b`, identified using the function key, with duplicates removed,
  -- and with the function `f` applied to elements that occur in both `a` and
  -- `b`. Work: O(`n`+`m`), span: O(1), assuming `key` and `f` have work
  -- complexity O(1).
  val union_by_key 't [m] [n] : (t -> i64) -> (t -> t -> t) -> [m]t -> [n]t -> ?[k].[k]t

  -- | `intersect_by_key key f a b` returns those elements in `a` that also
  -- appears in `b`, identified using the function `key`, with duplicates
  -- removed, and with the function `f` applied to the intersecting
  -- objects. Work: O(`n`+`m`), span: O(1), assuming `key` and `f` have work
  -- complexity O(1).
  val intersect_by_key 't [m] [n] : (t -> i64) -> (t -> t -> t) -> [m]t -> [n]t -> ?[k].[k]t

  -- | `diff_by_key key a b` returns `a` with duplicates and elements from `b`
  -- removed, where the function `key` is used for identifying objects. Work:
  -- O(`n`+`m`), span: O(1), assuming key has work complexity O(1).
  val diff_by_key 't [m] [n] : (t -> i64) -> [m]t -> [n]t -> ?[k].[k]t

  -- | `join_by_key key1 key2 a b` returns pairs of objects in `a` and `b` that
  -- agree on keys obtained with the `key1` and `key2` functions.
  val join_by_key 'a 'b [m] [n] : (a -> i64) -> (b -> i64) -> [m]a -> [n]b -> ?[k].[k](a, b)

  -- | `elimdups_by_key key f a` returns `a` with duplicates removed, identified
  -- using the function `key`, and with duplicates merged using the function
  -- `f`, which is assumed to be associative. Work: O(`n`), span: O(1), assuming
  -- `key` and `f` have work complexity O(1).
  val elimdups_by_key 't [m] : (t -> i64) -> (t -> t -> t) -> [m]t -> ?[k].[k]t
}

module setops : setops = {
  def unsome 'a f (x: opt a) : a =
    match x
    case #some x -> x
    case #none -> f ()

  def merge_opt 'a (f: a -> a -> a) (x: opt a) (y: opt a) : opt a =
    match (x, y)
    case (#some x, #some y) -> #some (f x y)
    case (#none, _) -> y
    case (_, #none) -> x

  def setop [m] [n] 'a (p: i64 -> bool) (f: a -> i64) (mrg: a -> a -> a) (A: [m]a) (B: [n]a) : ?[k].[k]a =
    let c: [m + n]a = A ++ B
    in map (\x -> (x, 1)) c
       |> radix_sort_by_key (\(x, _) -> f x) i64.num_bits i64.get_bit
       |> map (\(x, n) -> (#some x, n))
       |> scan (\(x, n) (y, m) ->
                  if equal_opt (\a b -> f a == f b) x y
                  then (merge_opt mrg x y, n + m)
                  else (y, m))
               (#none, 0)
       |> filter (\(_, n) -> p n)
       |> map (\(x, _) -> unsome (\_ -> c[0]) x)

  def union_by_key [m] [n] 't (key: t -> i64) (mrg: t -> t -> t) (a: [m]t) (b: [n]t) : ?[k].[k]t =
    setop (<= 1) key mrg a b

  def elimdups_by_key [m] 't (key: t -> i64) (mrg: t -> t -> t) (a: [m]t) : ?[k].[k]t =
    union_by_key key mrg [] a

  def intersect_by_key [m] [n] 't (key: t -> i64) (mrg: t -> t -> t) (a: [m]t) (b: [n]t) : ?[k].[k]t =
    let a = elimdups_by_key key mrg a
    let b = elimdups_by_key key mrg b
    in setop (> 1) key mrg a b

  def diff_by_key 't [m] [n] (key: t -> i64) (a: [m]t) (b: [n]t) : ?[k].[k]t =
    let xs =
      map (\x -> (x, 1)) (elimdups_by_key key (\x _ -> x) a)
      ++ map (\x -> (x, 2)) (elimdups_by_key key (\x _ -> x) b)
      |> radix_sort_by_key (\(x, _) -> key x) i64.num_bits i64.get_bit
    let sz = length xs
    let ks: [](opt t) =
      map (\i ->
             if i < sz - 1
             then if key (xs[i].0) == key (xs[i + 1]).0
                  then #none
                  else if xs[i].1 == 1
                  then #some (xs[i].0)
                  else #none
             else if xs[i].1 == 1
             then #some (xs[i].0)
             else #none)
          (indices xs)
    in ks |> filter is_some
       |> map (unsome (\_ -> a[0]))

  def union [m] [n] (a: [m]i64) (b: [n]i64) : ?[k].[k]i64 =
    union_by_key (\x -> x) (\x _ -> x) a b

  def elimdups [m] (a: [m]i64) : ?[k].[k]i64 =
    elimdups_by_key (\x -> x) (\x _ -> x) a

  def intersect [m] [n] (a: [m]i64) (b: [n]i64) : ?[k].[k]i64 =
    intersect_by_key (\x -> x) (\x _ -> x) a b

  def diff [m] [n] (a: [m]i64) (b: [n]i64) : ?[k].[k]i64 =
    diff_by_key (\x -> x) a b

  type beither 'a 'b = #left a | #right b | #both a b | #noone

  def keyb 'a 'b (key1: a -> i64) (key2: b -> i64) (x: beither a b) : i64 =
    match x
    case #left a -> key1 a
    case #right b -> key2 b
    case #both a _ -> key1 a
    case #noone -> -1

  def merge 'a 'b (key1: a -> i64) (key2: b -> i64) (x: beither a b) (y: beither a b) : beither a b =
    match (x, y)
    case (#left x', #right y') ->
      if key1 x' == key2 y'
      then #both x' y'
      else y
    case (_, #noone) -> x
    case (#noone, _) -> y
    case (_, #both _ _) -> y
    case (#both _ _, _) -> y
    case (#right _, _) -> y
    case (_, #left _) -> y

  def join_by_key [m] [n] 'a 'b (key1: a -> i64) (key2: b -> i64) (A: [m]a) (B: [n]b) : ?[k].[k](a, b) =
    map (\x -> #left x) A ++ map (\y -> #right y) B
    |> radix_sort_by_key (keyb key1 key2) i64.num_bits i64.get_bit
    |> scan (merge key1 key2) #noone
    |> filter (\m -> match m case #both _ _ -> true case _ -> false)
    |> map (\m -> match m case #both a b -> (a, b) case _ -> (A[0], B[0]))
}

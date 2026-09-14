-- | A collection of common binary reduction tree.
--
-- The following collection of binary reduction tree. These trees
-- allows for the abilities like asking questions about an array such
-- as "What is the next smaller or equal element in this array". Such
-- an question can be answered as following:
--
-- ```
-- module mintree = mk_mintree i32
-- let tree = mintree.make [16, 32, 64, 48, 42, 58, 17]
-- let i = mintree.next tree 1
-- ```
--
-- Here `i` will be the value `6` since that is the index of the next
-- smaller or equal element.
--
-- All these binary reduction trees are a specialisations of the
-- `transparent_reduction_tree` module which has no type safety
-- guarantees.

import "transparent_reduction_tree"

-- | Module type that specifies an ordered binary reduction tree.
module type ordered_reduction_tree = {
  -- | The type of elements in ordered binary reduction tree.
  type t

  -- | The type of the binary reduction tree.
  type~ tree

  -- | From an array of elements create a binary reduction tree.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(log n)*
  val make [n] : [n]t -> tree

  -- | Using the binary reduction tree and the index of an element
  -- find an element with a smaller index that fulfills some (<=)
  -- binary relation from a total ordering. If the element does not
  -- exists `-1` is returned. E.g. if it is a of minima you find the
  -- previous smaller or equal element. Likewise a of maxima finds the
  -- previous greater or equal element.
  --
  -- **Work:** *O(log n)*
  --
  -- **Span:** *O(1)*
  val previous : tree -> i64 -> i64

  -- | Using the binary reduction tree and the index of an element
  -- find an element with a greater index that fulfills some (<=)
  -- binary relation from a total ordering. If the element does not
  -- exists `-1` is returned.
  --
  -- **Work:** *O(log n)*
  --
  -- **Span:** *O(1)*
  val next : tree -> i64 -> i64

  -- | Using the binary reduction tree and the index of an element
  -- find an element with a smaller index that fulfills some (<)
  -- binary relation from a strict total ordering. If the element does
  -- not exists `-1` is returned.
  --
  -- **Work:** *O(log n)*
  --
  -- **Span:** *O(1)*
  val strict_previous : tree -> i64 -> i64

  -- | Using the binary reduction tree and the index of an element
  -- find anelement with a greater index that fulfills some (<) binary
  -- relation from a strict total ordering. If the element does not
  -- exists `-1` is returned.
  --
  -- **Work:** *O(log n)*
  --
  -- **Span:** *O(1)*
  val strict_next : tree -> i64 -> i64
}

-- | Make a binary reduction tree of minima.
module mk_mintree
  (O: {
    type t
    val highest : t
    val min : t -> t -> t
    val (<=) : t -> t -> bool
    val (<) : t -> t -> bool
  })
  : ordered_reduction_tree with t = O.t = {
  module T = transparent_reduction_tree
  type t = O.t
  type~ tree = ?[n].T.tree [n] t

  def make [n] (arr: [n]t) =
    T.make O.min O.highest arr

  def previous (tree: tree) (i: i64) : i64 =
    T.previous (O.<=) tree i

  def next (tree: tree) (i: i64) : i64 =
    T.next (O.<=) tree i

  def strict_previous (tree: tree) (i: i64) : i64 =
    T.previous (O.<) tree i

  def strict_next (tree: tree) (i: i64) : i64 =
    T.next (O.<) tree i
}

-- | Make a binary reduction tree of maxima.
module mk_maxtree
  (O: {
    type t
    val lowest : t
    val max : t -> t -> t
    val (>=) : t -> t -> bool
    val (>) : t -> t -> bool
  })
  : ordered_reduction_tree with t = O.t = {
  module T = transparent_reduction_tree
  type t = O.t
  type~ tree = ?[n].T.tree [n] t

  def make [n] (arr: [n]t) =
    T.make O.max O.lowest arr

  def previous (tree: tree) (i: i64) : i64 =
    T.previous (O.>=) tree i

  def next (tree: tree) (i: i64) : i64 =
    T.next (O.>=) tree i

  def strict_previous (tree: tree) (i: i64) : i64 =
    T.previous (O.>) tree i

  def strict_next (tree: tree) (i: i64) : i64 =
    T.next (O.>) tree i
}

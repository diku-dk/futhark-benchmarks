-- | An implementation of binary reduction tree.
--
-- A binary reduction is computed by taking an array and then
-- pairwise apply a binary operator. Continue doing this until a
-- single value is reached like in the tree reduction algorithm for
-- computing reduction. This tree can then be used to ask queries
-- about the surrounding elements. An example is you can construct a
-- binary tree of minima and find the previous smaller element in the
-- array. The module `mk_mintree` implements this strategy and uses
-- the module system to help garantee correctness. The implementation
-- found in here can allow for wrong usage and allows for the ability
-- to make more specialized modules.
--
-- The implementation is the one descriped by Bar-on and Vishkin [1]
-- and the implementation is based on the one found in Voetters
-- master thesis [2].
--
-- Sources:
-- [1] Ilan Bar-on and Uzi Vishkin. 1985. Optimal parallel generation
-- of a computation tree form. ACM Trans. Program. Lang. Syst. 7, 2
-- (April 1985), 348–357. https://doi.org/10.1145/3318.3478
-- [2] Voetter, Robin. 2021. Parallel lexing, parsing and semantic
-- analysis on the GPU. Master's thesis. Leiden University, Leiden,
-- The Netherlands.  https://theses.liacs. nl/2052

-- | Determine the height of the reduction tree from the array.
def height (n: i64) : i64 =
  let temp = i64.num_bits - i64.clz n
  in i64.i32 <| if i64.popc n == 1 then temp else temp + 1

-- | Determine the size from height of the reduction tree.
def size_from_height (h: i64) : i64 =
  (1 << h) - 1

-- | Determine the size of the reduction tree from the array.
def size (n: i64) : i64 =
  height n |> size_from_height

-- | Determine the sibling of a node in the reduction tree.
def sibling (i: i64) : i64 =
  i - i64.bool (i % 2 == 0) + i64.bool (i % 2 == 1)

-- | Determine the parent of a node in the reduction tree.
def parent (i: i64) : i64 =
  (i - 1) / 2i64

-- | Determine if the node is a left node in the reduction tree.
def is_left (i: i64) : bool =
  i % 2 == 1i64

-- | Determine if the node is a right node in the reduction tree.
def is_right (i: i64) : bool =
  i % 2 == 0i64

-- | Module type that specifies a reduction tree.
module type transparent_reduction_tree = {
  type tree [n] 't

  -- | Given a binary operation, a neutral element, and an array
  -- compute the binary reduction tree.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(log n)*
  val make [n] 't :
    (op: t -> t -> t) -> (ne: t) -> (arr: [n]t) -> tree [n] t

  -- | Given a binary relation, a tree, and an index find the first
  -- element with a smaller index which satisfies the relation. If it
  -- fails to find this element (-1) is the result.
  --
  -- **Work:** *O(log n)*
  --
  -- **Span:** *O(1)*
  val previous [n] 't :
    (op: t -> t -> bool) -> (tree: tree [n] t) -> (i: i64) -> i64

  -- | Given a binary relation, a tree, and an index find the first
  -- element with a greater index which satisfies the relation. If it
  -- fails to find this element (-1) is the result.
  --
  -- **Work:** *O(log n)*
  --
  -- **Span:** *O(1)*
  val next [n] 't :
    (op: t -> t -> bool) -> (tree: tree [n] t) -> (i: i64) -> i64

  -- | Given a tree return the tree as an array.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(1)*
  val to_array [n] 't : (tree: tree [n] t) -> [size n]t

  -- | Given an array on the tree form return the tree.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(1)*
  val from_array [n] 't : (array: [size n]t) -> tree [n] t
}

module transparent_reduction_tree : transparent_reduction_tree = {
  type tree [n] 't = {size: [n](), tree: [size n]t}

  def to_array [n] 't (tree: tree [n] t) : [size n]t =
    tree.tree

  def from_array [n] 't (tree: [size n]t) : tree [n] t =
    {size = rep (), tree}

  def make [n] 't (op: t -> t -> t) (ne: t) (arr: [n]t) : tree [n] t =
    let h = height n
    let tree_size = size_from_height h
    let offset = size_from_height (h - 1)
    let offsets = iota n |> map (+ offset)
    let tree = scatter (replicate tree_size ne) offsets arr
    let arr = copy tree[offset:]
    let (tree, _, _, _) =
      loop (tree, arr: *[tree_size - offset]t, level, new_size) =
             (tree, arr, h - 2, length arr)
      while level >= 0 do
        let new_size = new_size / 2
        let new_arr =
          scatter arr
                  (iota new_size)
                  (tabulate new_size (\i -> arr[2 * i] `op` arr[2 * i + 1]))
        let offset = size_from_height level
        let offsets = iota new_size |> map (+ offset)
        let new_tree = scatter tree offsets (take new_size new_arr)
        in (new_tree, new_arr, level - 1, new_size)
    in from_array (tree :> [size n]t)

  def previous [n] 't
               (op: t -> t -> bool)
               (tree: tree [n] t)
               (i: i64) : i64 =
    if i < 0 || n <= i
    then -1
    else let h = i64.i32 <| i64.num_bits - i64.clz (size n)
         let offset = size_from_height (h - 1)
         let start = offset + i
         let v = tree.tree[start]
         let ascent j = j != 0 && (is_left j || !(tree.tree[sibling j] `op` v))
         let descent j = 2 * j + 1 + i64.bool (tree.tree[2 * j + 2] `op` v)
         let index = iterate_while ascent parent start
         in if index != 0
            then iterate_while (< offset) descent (sibling index) - offset
            else -1

  def next [n] 't
           (op: t -> t -> bool)
           (tree: tree [n] t)
           (i: i64) : i64 =
    if i < 0 || n <= i
    then -1
    else let h = i64.i32 <| i64.num_bits - i64.clz (size n)
         let offset = size_from_height (h - 1)
         let start = offset + i
         let v = tree.tree[start]
         let ascent j = j != 0 && (is_right j || !(tree.tree[sibling j] `op` v))
         let descent j = 2 * j + 2 - i64.bool (tree.tree[2 * j + 1] `op` v)
         let index = iterate_while ascent parent start
         in if index != 0
            then iterate_while (< offset) descent (sibling index) - offset
            else -1
}

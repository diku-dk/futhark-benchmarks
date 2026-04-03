-- | Linked lists.
--
-- Perhaps surprisingly, linked lists actually work in a parallel
-- language.

module type list = {
  -- | A linked list with `n` elements of type `a`.
  type list [n] 'a

  -- | A partial function extracting the first element of the list.
  -- *O(1)*.
  val head [n] 'a : list [n] a -> a

  -- | A partial function extracting the last element of the list.
  -- *O(1)*.
  val last [n] 'a : list [n] a -> a

  -- | Reverse list.
  val rev [n] 'a : list [n] a -> list [n] a

  -- | Construct linked list from array. Work *O(n)*, span *O(1)*.
  val from_array [n] 'a : [n]a -> list [n] a

  -- | Convert linked list to array. Work *O(n log(n))*, span
  -- *O(log(n))*.
  val to_array [n] 'a : list [n] a -> [n]a

  -- `sub l i` retrieves the *i*th element of a list. Partial
  -- function. Work *O(i)*, span *O(i)*.
  val sub [n] 'a : list [n] a -> i64 -> a

  -- Concatenate two lists.  Work *O(n)*, span *O(1)*.
  val ++ [n] [m] 'a : list [n] a -> list [m] a -> list [n + m] a

  -- | Apply function to every element of list.
  val map [n] 'a 'b : (a -> b) -> list [n] a -> list [n] b

  -- | Inclusive scan of list, given an associative operator. Work
  -- *O(n log(n))*, span *O (log(n))*.
  val scan [n] 'a : (a -> a -> a) -> list [n] a -> list [n] a

  -- | Reduction with an associative operator.
  val reduce [n] 'a : (a -> a -> a) -> a -> list [n] a -> a

  -- | Reduction with an associative *and commutative* operator.
  val reduce_comm [n] 'a : (a -> a -> a) -> a -> list [n] a -> a
}

module list : list = {
  -- We use 'n' to denote end-of-list in S.
  type list [n] 'a =
    { S: [n]i64
    , V: [n]a
    , head: i64
    , last: i64
    }

  def head [n] 'a (l: list [n] a) = assert (n > 0) l.V[l.head]

  def last [n] 'a (l: list [n] a) = assert (n > 0) l.V[l.last]

  def rev [n] 'a (l: list [n] a) =
    let f i = (l.S[i], i)
    let (is, vs) = unzip (tabulate n f)
    in l with S = scatter (replicate n n) is vs
         with head = l.last
         with last = l.head

  def from_array 'a [n] (V: [n]a) : list [n] a =
    { S = map (+ 1) (iota n)
    , V
    , head = 0
    , last = n - 1
    }

  def wyllie_scan_step [n] 'a
                       (op: a -> a -> a)
                       (V: [n]a)
                       (S: [n]i64) =
    let f i =
      if S[i] == n
      then (V[i], S[i])
      else (V[i] `op` V[S[i]], S[S[i]])
    in unzip (tabulate n f)

  def wyllie_scan [n] 'a
                  (op: a -> a -> a)
                  (V: [n]a)
                  (S: [n]i64) =
    let (V, _) =
      loop (V, S) for _i < 64 - i64.clz n do
        wyllie_scan_step op V S
    in V

  def rank [n] (S: [n]i64) : [n]i64 = wyllie_scan (+) (replicate n 1) S

  def to_array [n] 'a (l: list [n] a) =
    scatter (copy l.V) (map (\i -> n - i) (rank l.S)) l.V

  def scan [n] 'a (op: a -> a -> a) (l: list [n] a) =
    let l' = rev l
    in l with V = (wyllie_scan op l'.V l'.S)

  def (++) [n] [m] 'a (x: list [n] a) (y: list [m] a) =
    { S =
        if n == 0 || m == 0
        then x.S ++ y.S
        else (copy x.S with [x.last] = n + y.head)
             ++ map (\i -> if i == n then n else i + n) y.S
    , V = x.V ++ y.V
    , head = x.head
    , last = y.last + n
    }

  def sub [n] 'a (l: list [n] a) i =
    assert (i >= 0 && i < n) l.V[iterate (i32.i64 i) (\j -> l.S[j]) 0]

  def reduce [n] 'a (op: a -> a -> a) (_ne: a) (l: list [n] a) =
    last (scan op l)

  def reduce_comm [n] 'a (op: a -> a -> a) (ne: a) (l: list [n] a) =
    reduce_comm op ne l.V

  def map [n] 'a 'b (f: a -> b) (l: list [n] a) : list [n] b =
    {S = l.S, V = map f l.V, last = l.last, head = l.head}
}

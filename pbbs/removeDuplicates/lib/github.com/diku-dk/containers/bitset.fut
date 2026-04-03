-- | Bitset module
--
-- A bitset data structure is an array of bits where a bit
-- can be set or not set. If the bit is set then it is a
-- member of the set otherwise it is not. The indexes of
-- these bits can then be related to the indexes of
-- another array.
--
-- `num_bits` is assumed to be constant in the time complexities.

local
def s (num_bits: i64) (n: i64) : i64 =
  (n - 1) / num_bits + 1i64

module type bitset = {
  -- | The bitset type.
  type bitset [n]

  -- | The number of bits for the chosen integral type.
  val num_bits : i64

  -- | Makes a empty bitset of a given capacity.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(1)*
  val empty : (n: i64) -> *bitset [s num_bits n]

  -- | Makes a singleton bitset with a given capacity.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(1)*
  val singleton : (n: i64) -> i64 -> *bitset [s num_bits n]

  -- | Checks if a bitset is empty.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(log n)*
  val is_empty [n] : bitset [s num_bits n] -> bool

  -- | Inserts a single bit in a bitset.
  --
  -- **Work:** *O(m)*
  --
  -- **Span:** *O(m)* (likely *O(1)*, the span comes from reduce by index.)
  val insert [m] [n] : *bitset [s num_bits n] -> [m]i64 -> bitset [s num_bits n]

  -- | Deletes a single bit in a bitset.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(m)* (likely *O(1)*, the span comes from reduce by index.)
  val delete [m] [n] : *bitset [s num_bits n] -> [m]i64 -> bitset [s num_bits n]

  -- | Checks if a bit is a member of a bitset.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val member [n] : i64 -> bitset [s num_bits n] -> bool

  -- | Bitset union.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(1)*
  val union [n] : bitset [s num_bits n] -> bitset [s num_bits n] -> *bitset [s num_bits n]

  -- | Bitset intersection.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(1)*
  val intersection [n] : bitset [s num_bits n] -> bitset [s num_bits n] -> *bitset [s num_bits n]

  -- | Bitset difference.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(1)*
  val difference [n] : bitset [s num_bits n] -> bitset [s num_bits n] -> *bitset [s num_bits n]

  -- | Checks if a bitset is a subset of another.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(1)*
  val is_subset [n] : bitset [s num_bits n] -> bitset [s num_bits n] -> bool

  -- | Finds the complement of a bitset.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(1)*
  val complement [n] : bitset [s num_bits n] -> *bitset [s num_bits n]

  -- | Sets the bitset capacity to a new value.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(1)*
  val set_capacity [m] : (n: i64) -> bitset [s num_bits m] -> *bitset [s num_bits n]

  -- | Computes the size of the set i.e. the population count.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(log n)*
  val size [n] : bitset [s num_bits n] -> i64

  -- | If a two bitsets contains the same bits then they are equal.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(log n)*
  val (==) [n] : bitset [s num_bits n] -> bitset [s num_bits n] -> bool

  -- | Convert an array of indices to a bitset.
  --
  -- **Work:** *O(m)*
  --
  -- **Span:** *O(m)* (likely *O(1)*, the span comes from reduce by index.)
  val from_array [m] : (n: i64) -> [m]i64 -> *bitset [s num_bits n]

  -- | Converts an array of u64 to a bitset.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val from_bit_array [m] : (n: i64) -> (arr: [m]u64) -> *bitset [s num_bits n]

  -- | Convert a bitset to an array of indices to a bitset.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(log n)*
  val to_array [n] : bitset [s num_bits n] -> *[]i64
}

-- | Creates a bitset module depending on a intergral type.
module mk_bitset (I: integral) : bitset = {
  def num_bits = i64.i32 I.num_bits

  module int = I
  type t = I.t
  type bitset [n] = [n]t

  def zero : t = I.u64 0

  def empty (n: i64) : *bitset [s num_bits n] =
    replicate (s num_bits n) zero

  def find_bitset_index (i: i64) (n: i64) : (i64, i32) =
    if i < 0 || n <= i
    then (-1, -1)
    else let num_bits = i64.i32 I.num_bits
         let j = i / num_bits
         let bit = i % num_bits
         in (j, i32.i64 bit)

  def insert [n] [m] (set: *bitset [s num_bits n]) (is: [m]i64) : bitset [s num_bits n] =
    let (is, bis) = map (flip find_bitset_index n) is |> unzip
    let vs = map (\i -> I.set_bit i zero 1) bis
    in reduce_by_index set (I.|) zero is vs

  def singleton (n: i64) (i: i64) : *bitset [s num_bits n] =
    let set = empty n
    let (j, bit) = find_bitset_index i n
    in if j == -1 || bit == -1
       then set
       else set with [j] = I.set_bit bit zero 1

  def is_empty [n] (set: bitset [s num_bits n]) : bool =
    all (I.== zero) set

  def delete [n] [m] (set: *bitset [s num_bits n]) (is: [m]i64) : bitset [s num_bits n] =
    let (is, bis) = map (flip find_bitset_index n) is |> unzip
    let vs = map (\i -> I.not (I.set_bit i zero 1)) bis
    in reduce_by_index set (I.&) zero is vs

  def member [n] (i: i64) (s: bitset [s num_bits n]) : bool =
    let (i, bit) = find_bitset_index i n
    in if i < 0 || bit < 0
       then false
       else I.get_bit bit s[i] == 1

  def union [n] (a: bitset [s num_bits n]) (b: bitset [s num_bits n]) : *bitset [s num_bits n] =
    map2 (I.|) a b

  def intersection [n] (a: bitset [s num_bits n]) (b: bitset [s num_bits n]) : *bitset [s num_bits n] =
    map2 (I.&) a b

  def set_trailing_bits_zero [n] (set: bitset [s num_bits n]) : bitset [s num_bits n] =
    let len = length set
    let unused_bits = u64.i64 (num_bits * len - n)
    let to_keep = u64.not ((1u64 << unused_bits) - 1u64)
    in map2 (\i b ->
               if i == len - 1 && unused_bits != 0
               then b I.& I.u64 to_keep
               else b)
            (indices set)
            set

  def complement [n] (set: bitset [s num_bits n]) : *bitset [s num_bits n] =
    map I.not set
    |> set_trailing_bits_zero

  def size [n] (s: bitset [s num_bits n]) : i64 =
    map (i64.i32 <-< I.popc) s
    |> i64.sum

  def (==) [n] (a: bitset [s num_bits n]) (b: bitset [s num_bits n]) : bool =
    map2 (I.==) a b
    |> and

  def is_subset [n] (a: bitset [s num_bits n]) (b: bitset [s num_bits n]) : bool =
    (a `union` b) == b

  def difference [n] (a: bitset [s num_bits n]) (b: bitset [s num_bits n]) : *bitset [s num_bits n] =
    a `intersection` complement b

  def set_capacity [m] (n: i64) (set: bitset [s num_bits m]) : *bitset [s num_bits n] =
    let s' = empty n
    let len = length set
    in map (\i ->
              if i < len then set[i] else zero)
           (indices s')
       |> set_trailing_bits_zero

  def from_bit_array [m] (n: i64) (arr: [m]u64) : *bitset [s num_bits n] =
    map (I.u64) arr
    |> sized (s num_bits n)
    |> set_trailing_bits_zero

  def from_array [m] (n: i64) (arr: [m]i64) : *bitset [s num_bits n] =
    copy (insert (empty n) arr)

  def to_array [n] (s: bitset [s num_bits n]) : *[]i64 =
    map2 (\i v ->
            let m = i * i64.i32 I.num_bits
            in map (\bit ->
                      if I.get_bit (i32.i64 bit) v i32.== 1
                      then m + bit
                      else -1)
                   (iota (i64.i32 I.num_bits)))
         (indices s)
         s
    |> flatten
    |> filter (0 <=)
}

module bitset = mk_bitset u64
module bitset_u32 = mk_bitset u32

-- | Abstract interface for working with sets.
--
-- For implementations of this interface, see:
--
-- * `mk_hashset`@term@"hashset"

-- | A general module type for a key-value map. Specific implementation of this
-- module may provide additional functionality, and may in particular deviate
-- significantly in the efficiency of individual operations.
--
-- For the meaning of the context type, see `map`@mtype@"map".
module type set = {
  -- | The key type.
  type key

  -- | The context type.
  type ctx

  -- | The type of sets of size `n`.
  type~ set [n]

  -- | Check if a key is member of the set.
  val member [n] : ctx -> key -> set [n] -> bool

  -- | Check if a key is not member of the set
  val not_member [n] : ctx -> key -> set [n] -> bool

  -- | Given an array keys construct a set.
  val from_array [u] : ctx -> [u]key -> ?[n].set [n]

  -- | Given an array keys construct a set. If the given keys
  -- contains duplicates then the function call will never finish.
  -- Inturn it does less work.
  val from_array_nodup [u] : ctx -> [u]key -> ?[n].set [n]

  -- | Convert set to an array of keys.
  val to_array [n] : set [n] -> [n]key

  -- | The number of elements in the set (`n`).
  val size [n] : set [n] -> i64

  -- | Gets the context of the hashmap.
  val context [n] : set [n] -> ctx

  -- | Insert new keys into a set. If a key already exists in the
  -- set, the new value will overwrite the old one.
  val insert [n] [u] :
    ctx -> set [n] -> [u]key -> ?[n'].set [n']
}

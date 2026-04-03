-- | Abstract interface for working with key-value mappings.
--
-- For implementations of this interface, see:
--
-- * `mk_hashmap`@term@"hashmap"
--
-- * `mk_arraymap`@term@"arraymap"

import "opt"

-- | A general module type for a key-value map. Specific implementation of this
-- module may provide additional functionality, and may in particular deviate
-- significantly in the efficiency of individual operations.
--
-- The module involves the notion of a *context*, which is used when performing
-- operations on keys, such as comparisons or hashing (depending on the
-- implementation of the map), which requires information beyond what is
-- available in the key values themselves. For example, consider a map where the
-- keys are string slices represented as pairs of integers, denoting offsets and
-- lengths into some larger string. In such cases it is only meaningful to
-- compare slices if we also have access to the larger string. A map thus
-- contains a context that is associated with all the keys in that map. However,
-- operations such as `lookup` will take an additional context argument, which
-- is used for those keys that are not already present of the mapping.
--
-- In those cases where the key type contains all pertinent information (e.g. if
-- the keys are just integers), the context can be `()` or some other
-- informationless type.
module type map = {
  -- | The key type.
  type key

  -- | The context type.
  type ctx

  -- | The map type, which contains `n` mappings of keys to values of type `v`.
  type~ map [n] 'v

  -- | Check if a key is member of the map.
  val member [n] 'v : ctx -> key -> map [n] v -> bool

  -- | Check if a key is not member of the map
  val not_member [n] 'v : ctx -> key -> map [n] v -> bool

  -- | Look up a value.
  val lookup [n] 'v : ctx -> key -> map [n] v -> opt v

  -- | Given a key-value array construct a map.
  val from_array [u] 'v :
    ctx -> [u](key, v) -> ?[n].map [n] v

  -- | Create map with default value.
  val from_array_rep [u] 'v :
    ctx
    -> [u]key
    -> v
    -> ?[n].map [n] v

  -- | Create map where duplicates are reduced with a commutative
  -- and associative operation.
  val from_array_hist [u] 'v :
    ctx
    -> (v -> v -> v)
    -> v
    -> [u](key, v)
    -> ?[n].map [n] v

  -- | Given a key-value array construct a map, assuming no duplicate keys.
  val from_array_nodup [n] 'v :
    ctx -> [n](key, v) -> map [n] v

  -- | Create map with default value, assuming no duplicate keys.
  val from_array_rep_nodup [n] 'v :
    ctx
    -> [n]key
    -> v
    -> map [n] v

  -- | Combine key-value pairs into a map using the provided
  -- associative and commutative operation. Keys that are not present
  -- in the map is not added.
  val adjust [n] [u] 'v :
    (v -> v -> v)
    -> v
    -> map [n] v
    -> [u](key, v)
    -> map [n] v

  -- | Map a function over the map values.
  val map [n] 'a 'b :
    (g: a -> b) -> map [n] a -> map [n] b

  -- | Map a function over the map values.
  val map_with_key [n] 'a 'b :
    (g: key -> a -> b)
    -> map [n] a
    -> map [n] b

  -- | Convert map to an array of key-value pairs.
  val to_array [n] 'v : map [n] v -> [](key, v)

  -- | Updates the value of the hash map using the key with the
  -- smallest index. No new keys are added.
  val update [n] [u] 'v :
    map [n] v
    -> [u](key, v)
    -> map [n] v

  -- | The number of values in the map (same as `n`).
  val size [n] 'v : map [n] v -> i64

  -- | Gets the context of the map.
  val context [n] 'v : map [n] v -> ctx

  -- | Insert new key-value pairs into a map. If a key already exists in the
  -- map, the new value will overwrite the old one.
  val insert [n] [u] 'v :
    ctx
    -> map [n] v
    -> [u](key, v)
    -> ?[m].map [m] v

  -- | Insert new key-value pairs into a map, combining values of duplicate keys
  -- using the provided associative and commutative operation.
  val insert_with [n] [u] 'v :
    ctx
    -> (v -> v -> v)
    -> v
    -> map [n] v
    -> [u](key, v)
    -> ?[m].map [m] v

  -- | Reduce values into a single value by an associative and
  -- commutative operator.
  val reduce [n] 'v : (v -> v -> v) -> v -> map [n] v -> v
}

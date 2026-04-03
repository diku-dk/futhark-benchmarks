-- | A module type for keys that can be hashed.

-- | This module specifies what key needs to be defined for it to be used in
-- data structures which use hash functions, such as
-- `mk_hashmap`@term@"hashmap".
module type hashkey = {
  -- | Context type.
  type~ ctx

  -- | Key type.
  type key

  -- | The constant type for the hash function.
  type const

  -- | The hash type.
  type hash

  -- | The number generator state.
  type rng

  -- | Initialise an RNG state from a seed.
  val rng_from_seed [n] : [n]i32 -> rng

  -- | Generate random constants for the hash function.
  val rand : rng -> (rng, const)

  -- | Equality definition for the key.
  val (==) : (ctx, key) -> (ctx, key) -> bool

  -- | A given hash function use.
  val hash : ctx -> const -> key -> hash
}

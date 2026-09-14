-- | Ordered key type.

module type ordkey = {
  -- | Context type.
  type~ ctx

  -- | Key type.
  type key

  -- | Less-than-or-equal.
  val (<=) : (ctx, key) -> (ctx, key) -> bool

  -- | Equality. This can be defined in terms of `<=`, but sometimes a more
  -- efficient implementation is possible.
  val (==) : (ctx, key) -> (ctx, key) -> bool
}

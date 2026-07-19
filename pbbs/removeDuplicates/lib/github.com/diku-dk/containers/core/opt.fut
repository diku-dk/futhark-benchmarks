-- | A module for working with optional values.
--
-- The `opt` type is either `#some` containing a value or `#none`.

-- | Either `#some` containing a value `a`@term or `#none`.
type opt 'a = #none | #some a

module opt = {
  -- | Extends a binary operation to have `#none` as an identity element.
  --
  -- This can be used when an associative operation has [no neutral
  -- element](https://futhark-lang.org/examples/no-neutral-element.html).
  def add_identity 'a (op: a -> a -> a) (a: opt a) (b: opt a) : opt a =
    match (a, b)
    case (#some a', #some b') -> #some (a' `op` b')
    case (#some _, #none) -> a
    case (#none, #some _) -> b
    case (#none, #none) -> #none

  -- | Unpacks an `opt` value.
  --
  -- If `#some a` then `a` is returned, otherwise `ne` is returned.
  def from 'a (ne: a) (a: opt a) : a =
    match a
    case #some a' -> a'
    case #none -> ne

  -- | Maps a function inside `opt`.
  --
  -- Applies the function `f` to the value `'a`.
  def map 'a 'b (f: a -> b) (x: opt a) : opt b =
    match x
    case #none -> (#none : opt b)
    case #some x' -> #some (f x')

  -- | Definition of `opt` equality.
  --
  -- The equality holds if they are both `#none` or they are both
  -- `#some` and the values inside `#some` are equal.
  def equal 'a (eq: a -> a -> bool) (a: opt a) (b: opt a) : bool =
    match (a, b)
    case (#some a', #some b') -> a' `eq` b'
    case (#none, #none) -> true
    case _ -> false

  -- | Maps a value to an `opt` type.
  --
  -- This is syntactic sugar for `#some a`, it may be nicer to use than
  -- a lambda function.
  def some 'a (a: a) : opt a =
    #some a

  -- | A `#none` value, symmetric with `some`.
  def none 'a : opt a = #none

  -- | `is_some a` is true if the constructor is `#some`.
  def is_some 'a (a: opt a) : bool =
    match a
    case #some _ -> true
    case _ -> false

  -- | `is_none a` is true when the constructor is `#none`.
  def is_none 'a (a: opt a) : bool =
    is_some a
    |> not

  -- | The bind operation for the optional type.
  def bind 'a 'b (f: a -> opt b) (a: opt a) : opt b =
    match a
    case #some a -> f a
    case #none -> #none

  -- | Find the first `#some` element if one exists.
  def first_some 't (xs: [](opt t)) : opt t =
    reduce (\x y ->
              match (x, y)
              case (#some x, _) -> #some x
              case (_, #some y) -> #some y
              case _ -> #none)
           #none
           xs
}

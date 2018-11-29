-- | Efficient statically sized vectors.
--
-- This file defines a module type, `vector`@mtype, that describes
-- single-dimensional vectors with an abstract interface.  The point
-- is that the interface has been designed such that it can be
-- implemented both with an ordinary Futhark array
-- (`any_vector`@term), but also with a tuple-representation for those
-- cases where the size of the vector is known statically
-- (`vector_1`@term and `cat_vector`@term).  This is useful for small
-- vectors, because they will then be kept in registers rather than
-- memory.  It also makes it explicit to the compiler that the
-- potential parallelism provided by the vector operations is not
-- worth exploiting.  This is generally the case when working with
-- many points in some space.
--
-- ## Examples
--
-- The `vector_1`@term module provides us with single-element vectors.
-- We can build those up with `cat_vector` to produce vectors of any
-- (static) size:
--
-- ```
-- module vector_2 = cat_vector vector_1 vector_1
-- module vector_3 = cat_vector vector_2 vector_1
-- module vector_5 = cat_vector vector_2 vector_3
-- module vector_8 = cat_vector vector_5 vector_3
--
-- let main (xs: [8]i32) =
--   xs
--   |> vector_8.from_array
--   |> vector_8.map (+1)
--   |> vector_8.to_array
-- ```
--
-- This is awkward for very long vectors, but these would not be
-- efficient anyway.

-- | A module type describing a one-dimensional vector (although a
-- length-n vector may well represent a point in an n-dimensional
-- space).  Note the absence of run-time vector concatenation - this
-- is omitted to facilitate statically sized representations, such as
-- tuples.
module type vector = {
  type vector 'a

  -- | Apply a function to every element of the vector.
  val map 'a 'b: (a -> b) -> vector a -> vector b

  -- | Perform a reduction of the vector with an associative operator
  -- that has the provided neutral element.
  val reduce 'a : (a -> a -> a) -> a -> vector a -> a

  -- | Construct a vector of pairs from two vectors.
  val zip 'a 'b : vector a -> vector b -> vector (a,b)

  -- | Retrieve the element at some position.
  val get 'a: i32 -> vector a -> a

  -- | Set the element at some position.
  val set 'a: i32 -> a -> vector a -> vector a

  -- | The length of a vector.
  val length 'a : vector a -> i32

  -- | Convert a vector to an array.
  val to_array 'a: vector a -> []a

  -- | Create a vector from an array.
  val from_array 'a : []a -> vector a
}

-- | An implementation of `vector`@mtype that uses an ordinary array
-- as the representation.  This is efficient for large vectors and the
-- implementation of `map` and `reduce` is parallel.  However, the
-- `set` operation is very slow for this implementation, as it
-- requires a full copy of the vector.  Unless you are interacting
-- with a parametric module that specifically requires an
-- implementation of `vector`@mtype, it is probably better to use
-- arrays directly than to go through this module.
--
-- Since the size of the vectors is not constrained by the type, it is
-- possible for `zip` to fail with a size error, just as with the
-- ordinary `zip`@term@"/futlib/array".
module any_vector : vector with vector 'a = []a = {
  type vector 'a = []a

  let empty _ = []
  let map = map
  let reduce = reduce
  let zip = zip
  let get i a = a[i]
  let set i v a = copy a with [i] = v
  let length = length
  let to_array = id
  let from_array = id
}

-- | A module implementing statically-sized single-element vectors.
-- The implementation of `map` and `reduce` is sequential.
module vector_1 : vector = {
  type vector 'a = a

  let map f a = f a
  let reduce f ne a = f ne a
  let zip a b = (a, b)
  let get _ a = a
  let set _ x _ = x
  let length _ = 1i32
  let to_array a = [a]
  let from_array as = as[0]
}

-- | Concatenation of statically sized vector modules.  This is used
-- to build up statically sized vectors of some size.  For example,
-- `cat_vector vector_1 vector_1` produces a module that defines
-- length-2 vectors.  The implementation of `map` and `reduce` is
-- sequential.
module cat_vector (X: vector) (Y: vector): vector = {
  type vector 'a = (X.vector a, Y.vector a)

  let map f (xs, ys) = (X.map f xs, Y.map f ys)
  let reduce f ne (xs, ys) = X.reduce f ne xs `f` Y.reduce f ne ys
  let zip (xs_a, ys_a) (xs_b, ys_b) = (X.zip xs_a xs_b, Y.zip ys_a ys_b)
  let get i (xs, ys) = if i < X.length xs then X.get i xs else Y.get (i-X.length xs) ys
  let set i v (xs, ys) = if i < X.length xs then (X.set i v xs, ys)
                         else (xs, Y.set (i-X.length xs) v ys)
  let length (xs, ys) = X.length xs + Y.length ys
  let to_array (xs, ys) = X.to_array xs ++ Y.to_array ys
  let from_array as = let xs = X.from_array as
                      let ys = Y.from_array as[X.length xs:]
                      in (xs, ys)
}

-- | Facilities for constructing one-hot values suitable for use with
-- AD.
--
-- A one-hot vector (or more generally, a one-hot value) is a value
-- that is zero everywhere except at a specific point.
--
-- Operationally, constructing a one-hot vector is simply a
-- `tabulate`@term where the index is inspected to produce either 0 or
-- 1. However, when working with more complicated data types, such as
-- pairs, records, or nested arrays, this can get quite verbose, and
-- the index arithmetic is easy to get wrong. This file provides
-- helper functions for building one-hot value generators.

module type onehot = {
  -- | A generator that produces values of type `a` in a space of size
  -- `n`. The space indicates the number of different positions that
  -- exist in a value of type `a`. For example, if `a` is a vector
  -- with *n* elements, the size of the space would be *n*. If a
  -- generator is asked to produce a value outside of its space, the
  -- result must be entirely zero.
  type^ gen [n] 'a

  -- | Generate a one-hot value that is zero everywhere except at position 'i'.
  val onehot [n] 'a : gen [n] a -> (i: i64) -> a

  -- | The size of the generation space; equivalently, the size of a
  -- value of type 'a'.
  val size [n] 'a : gen [n] a -> i64

  -- | Resize the generation space. This does not affect the actual
  -- values generated.
  val resize [n][m] 'a : gen [n] a -> gen [m] a

  -- | Produces a generator that is `one` at index 0 and `zero`
  -- everywhere else.
  val point 'a : (one: a) -> (zero: a) -> gen [1] a

  val bool : gen [1] bool

  val i8 : gen [1] i8
  val i16 : gen [1] i16
  val i32 : gen [1] i32
  val i64 : gen [1] i64

  val u8 : gen [1] u8
  val u16 : gen [1] u16
  val u32 : gen [1] u32
  val u64 : gen [1] u64

  val f16 : gen [1] f16
  val f32 : gen [1] f32
  val f64 : gen [1] f64

  -- | A generator for a fixed value that (as far as generation is
  -- concerned) contains no other values. This is useful for input or
  -- output that is known not to contribute to the derivative.
  val fixed 'a : a -> gen [0] a

  -- | Produce a generator for pairs based on generators for the
  -- components.
  val pair [n][m] 'a 'b : gen [n] a -> gen [m] b -> gen [n+m] (a,b)

  -- | Produce a generator for triples based on generators for the
  -- components.
  val triple [n][m][k] 'a 'b 'c : gen [n] a -> gen [m] b -> gen [k] c -> gen [n+m+k] (a,b,c)

  -- | A generator for arrays, given a generator for the elements.
  -- Polymorphic in the array size, which will be inferred at the
  -- usage site.
  val arr [n][m] 'a : gen [m] a -> gen [n*m] ([n]a)

  -- | Repeats the elements of a generator.
  val cycle [n][r] 'a : gen [n] a -> gen [n] ([r]a)
}

module onehot : onehot = {
  type^ gen [n] 'a = {
      size: [0][n](),
      gen: i64 -> a
    }

  def witness x = [] : [0][x]()

  def onehot 'a (gen: gen [] a) i = gen.gen i
  def size [n] 'a (_: gen [n] a) = n

  def resize [n][m] 'a (gen: gen [n] a) = {size = witness m, gen = gen.gen}

  def point one zero = {size = witness 1,
                        gen = \i -> if i == 0i64 then one else zero}
  def fixed a = { size = witness 0, gen = const a }

  def pair [n][m] 'a 'b (x: gen[n]a) (y: gen[m]b) =
    { size = witness (n+m),
      gen = \i -> (onehot x i,
                   onehot y (i-n))
    }

  def triple [n][m][k] 'a 'b 'c (a:gen[n]a) (b:gen[m]b) (c:gen[k]c) =
    { size = witness (n+m+k)
    , gen = \i -> let ((a',b'),c') = onehot (pair (pair a b) c) i
                  in (a',b',c')
    }

  def arr [n][m] 'a (gen: gen [m] a) =
    { size = witness (n*m),
      gen = \i -> tabulate n (\l ->
                                if i / m == l
                                then onehot gen (i %% m)
                                else onehot gen (-1))
    }

  def cycle [n][r] 'a (gen: gen [n] a) =
    { size = witness n,
      gen = \i -> replicate r (gen.gen (if i < 0 then -1 else i%%n))
    }

  def bool = point true false
  def i8 = point 1i8 0i8
  def i16 = point 1i16 0i16
  def i32 = point 1i32 0i32
  def i64 = point 1i64 0i64
  def u8 = point 1u8 0u8
  def u16 = point 1u16 0u16
  def u32 = point 1u32 0u32
  def u64 = point 1u64 0u64

  def f16 = point 1f16 0f16
  def f32 = point 1f32 0f32
  def f64 = point 1f64 0f64
}

-- | Generate all one-hot values possible for a given generator.
def onehots [n] 'a (gen: onehot.gen [n] a) : [n]a =
  tabulate n (onehot.onehot gen)

-- | Compute the gradient of a function given a one-hot generator for its result.
def grad gen f x = map (vjp f x) (onehots gen)

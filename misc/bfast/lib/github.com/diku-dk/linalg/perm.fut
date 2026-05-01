-- | Permutations. Permutations can be constructed using the `id` and `mk`
-- constructors and manipulated using a series of other constructors, which all
-- guarantee that compositions are safe. The only way to inspect a permutation
-- is to apply it, using the `permute` function.

local module type perm = {

  -- | Type of permutations
  type t [n]

  -- | The identity permutation of size `n`.
  val id : (n:i64) -> t[n]

  -- | Parallel composition of permutations
  val add [m] [n] : t[m] -> t[n] -> t[m+n]

  -- | Build a permutation from another permutation and a swap. It is checked
  -- that the two indices are in-bound.
  val swap [n] : t[n] -> i64 -> i64 -> t[n]

  -- | Composition of two permutations. We have `permute (compose f g) a =
  -- permute f (permute g a)`.
  val compose [n] : t[n] -> t[n] -> t[n]

  -- | The inverse permutation. We have `permute (inv p) (permute p a) = a` for
  -- any `p` and any `a` of compatible sizes.
  val inv [n] : t[n] -> t[n]

  -- | Build a permutation from a vector of indices indicating their
  -- position. We have `permute (mk is) a = map (\i -> a[i]) is`. It is checked
  -- that `is` indeed is a permutation.
  val mk [n] : [n]i64 -> t[n]

  -- | Perform a permutation on a vector.
  val permute 'a [n] : t[n] -> [n]a -> [n]a

  -- | Reverse a permutation.
  val rev [n] : t[n] -> t[n]
}

module perm : perm = {
  type t[n] = [n]i64
  def id (n:i64) = iota n
  def add [m][n] (p:t[m]) (p':t[n]) : t[m+n] =
    p ++ (map (+m) p')
  def swap [n] (p:t[n]) (i:i64) (j:i64) : t[n] =
    assert (0 <= i && i < n && 0 <= j && j < n)
	   (let tmp = p[i]
	    in (copy p with [i] = p[j]) with [j] = tmp)
  def permute 'a [n] (p:t[n]) (v:[n]a) : [n]a =
    map (\i -> v[i]) p
  def compose [n] (f:t[n]) (g:t[n]) : t[n] =
    permute f g
  def inv [n] (p:t[n]) : t[n] =
    scatter (iota n) p (iota n)
  def eqvi [n] (x:[n]i64) (y:[n]i64) =
    map2 (i64.==) x y |> reduce (&&) true
  def ERROR_expecting_permutation x = x
  def mk [n] (p:[n]i64) : t[n] =
    assert (ERROR_expecting_permutation (eqvi (permute (inv p) (permute p (iota n))) (iota n)))
	   (p)
  def rev [n] (p:t[n]) : t[n] = reverse p
}

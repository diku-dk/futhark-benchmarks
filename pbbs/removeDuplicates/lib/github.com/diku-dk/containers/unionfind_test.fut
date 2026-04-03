import "unionfind"
import "../sorts/merge_sort"
import "opt"

module unionfind_sequential : unionfind = {
  type handle = i64

  type unionfind [n] =
    {parents: [n]handle}

  def none : handle = i64.highest

  def to_i64 [n] (_: unionfind [n]) (h: handle) : i64 =
    assert (0 <= h && h <= n) h

  def from_i64 [n] (_: unionfind [n]) (i: i64) : handle =
    assert (0 <= i && i <= n) i

  #[sequential]
  def handles [n] (_: unionfind [n]) : *[n]handle =
    iota n

  #[sequential]
  def create (n: i64) : *unionfind [n] =
    {parents = rep none}

  #[sequential]
  def find_one [n] (uf: unionfind [n]) h =
    loop h while uf.parents[h] != none do
      uf.parents[h]

  #[sequential]
  def find [n] [u]
           (uf: *unionfind [n])
           (hs: [u]handle) : (*unionfind [n], [u]handle) =
    let hs' = map (find_one uf) hs
    in (uf, hs')

  #[sequential]
  def find' [n] [u]
            (uf: unionfind [n])
            (hs: [u]handle) : *[u]handle =
    map (find_one uf) hs

  #[sequential]
  def union [n] [u]
            (uf: unionfind [n])
            (eqs: [u](handle, handle)) : *unionfind [n] =
    loop uf' = copy uf
    for (h, h') in eqs do
      let (i, p) = (find_one uf' h, find_one uf' h')
      in if i == p
         then uf'
         else uf' with parents[i] = p
}

module type norm_eq_count = {
  -- | Normalized equivalence count will create an initial union find
  -- structures of size `n`@norm_eq_count then uses the
  -- `eqs`@norm_eq_count to index into the array of handles to create
  -- the array of equations that are used to be unioned. Then the
  -- number of equivalent elements are counted where the element with
  -- the smallest index is used to be the representative.
  val norm_eq_count [m] : (n: i64) -> (eqs: [m](i64, i64)) -> [n]i64
}

module mk_norm_eq_count
  (U: unionfind)
  : norm_eq_count = {
  type t = U.handle

  def normalize [n] (uf: U.unionfind [n]) (hs: [n]t) : [n]i64 =
    let eq = equal_opt (\a b -> U.to_i64 uf a == U.to_i64 uf b)
    let is =
      map (\h ->
             map some hs
             |> zip (indices hs)
             |> reduce_comm (\a b ->
                               if a.0 != -1 && b.0 != -1
                               then if a.1 `eq` some h
                                    then a
                                    else b
                               else a)
                            (-1, #none))
          hs
      |> map (.0)
    in is

  def norm_eq_count [m]
                    (n: i64)
                    (eqs: [m](i64, i64)) : [n]i64 =
    let uf = U.create n
    let hs = U.handles uf
    let eqs' = map (\(i, j) -> (hs[i % n], hs[j % n])) eqs
    let uf = U.union uf eqs'
    let (uf, ps) = U.find uf hs
    let reps = normalize uf ps
    in hist (+) 0 n reps (rep 1)
}

module type test = {
  -- | Test that two unionfind implementations give an equivalent
  -- partioning of variables.
  val test : i64 -> i64 -> bool
}

module mk_test_params (U0: unionfind) (U1: unionfind) : test = {
  module u0_norm_eq_count = mk_norm_eq_count U0
  module u1_norm_eq_count = mk_norm_eq_count U1

  -- | Multiply-shift hash function https://arxiv.org/abs/1504.06804
  def hash (a: (u64, u64)) (b: (u64, u64)) (x: u64) : u64 =
    let y_mul_lo = a.0 * x
    in u64.mul_hi a.1 x + b.1 + u64.bool (y_mul_lo < b.0)

  def random (n: i64) : [n]i64 =
    let seed = (n + 2677) * 27644437
    let a0 = 0x8422d1795f837e8b
    let a1 = 0x78f81f96f93e6ca9
    let b0 = 0x643518302a112aa1
    let b1 = 0x58fe49a7f968fbe7
    in iota n
       |> map (i64.u64
               <-< hash (a0, a1) (b0, b1)
               <-< u64.i64
               <-< (+ seed))

  def equations (n: i64) (m: i64) : [m](i64, i64) =
    random (m + m)
    |> map (% n)
    |> split
    |> uncurry zip

  def test (num_vars: i64) (num_eqs: i64) : bool =
    let eqs = equations num_vars num_eqs
    let expected = u0_norm_eq_count.norm_eq_count num_vars eqs
    let result = u1_norm_eq_count.norm_eq_count num_vars eqs
    in map2 (==) expected result
       |> and
}

module mk_test = mk_test_params unionfind_sequential
module test_unionfind = mk_test unionfind
module test_unionfind_by_size = mk_test unionfind_by_size
module test_unionfind_by_rank = mk_test unionfind_by_rank

-- ==
-- entry: unionfind_test unionfind_by_size_test unionfind_by_rank_test
-- input { 10000i64 2000i64 }
-- output { true }
-- input { 10000i64 40000i64 }
-- output { true }
entry unionfind_test (num_vars: i64) (num_eqs: i64) : bool =
  test_unionfind.test num_vars num_eqs

entry unionfind_by_size_test (num_vars: i64) (num_eqs: i64) : bool =
  test_unionfind_by_size.test num_vars num_eqs

entry unionfind_by_rank_test (num_vars: i64) (num_eqs: i64) : bool =
  test_unionfind_by_rank.test num_vars num_eqs

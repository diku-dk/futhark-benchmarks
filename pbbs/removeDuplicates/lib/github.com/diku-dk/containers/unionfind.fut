-- | Module containing different implementations of union-find.
--
-- Union-find is a data structure which support three operation:
-- * `create`@term: Make an empty union-find structure.
-- * `find`@term: Find the representative of an element in the
--   union-find structure.
-- * `union`@term: Make it so to elements in the union-find structure
--   has the same representative.
--
-- The implementations found in this library focuses on bulk parallel
-- find and union operations. There are three different
-- implementations of the parallel union-find data-structure.
-- * `unionfind`@term: A bare bones union-find implementation which
--   has good performance if about O(log n) calls of union is done
--   where `n` is the number of element pairs to be unioned.  If you
--   have linear amount of calls to union then the finds will become
--   expensive and linear in cost.
-- * `union_by_rank`@term: A union-find implementation which uses union
--   by rank leading to the find being O(log n) for a single element
--   where `n` is the number of element pairs that have been unioned.
-- * `union_by_size`@term: A union-find implementation which uses
--   union by size leading to the same properties as
--   `union_by_rank`@term but it has worse performance when
--   benchmarking the union operation.

import "hyperloglog"
import "hashkey"
import "key"

module type unionfind = {
  -- | A handle is an element in the union-find structure.
  type handle

  -- | The union-find structure.
  type unionfind [n]

  -- | Create an union-find structure of `n`@term handles where
  -- initially every element is not unioned with no other element then
  -- it self.
  val create : (n: i64) -> *unionfind [n]

  -- | Given an array of handles find the representative of each
  -- handle and give back the new union-find structure with the
  -- representatives.
  val find [n] [u] : *unionfind [n] -> [u]handle -> *(unionfind [n], [u]handle)

  -- | Given an array of handles find the representative of each
  -- handle. The unionfind structure will remain unchanged, this may
  -- ruin asymptotic garantees.
  val find' [n] [u] : unionfind [n] -> [u]handle -> *[u]handle

  -- | Perform an union between multiple handles, every tuple pair
  -- will be unioned to have the same representative.
  val union [n] [u] : *unionfind [n] -> [u](handle, handle) -> *unionfind [n]

  -- | Retrieve all handles.
  val handles [n] : unionfind [n] -> *[n]handle

  -- | Lookup the handle found at an in bound index in the array
  -- created by the `handles`@term function. If the index is not in
  -- bound an error will be thrown.
  val from_i64 [n] : unionfind [n] -> i64 -> *handle

  -- | Lookup the index of a handle found in the array created by the
  -- `handles`@term function. If the handle is not from a union-find
  -- structure that is less than or equal to `n`@term then an error
  -- will be thrown.
  val to_i64 [n] : unionfind [n] -> handle -> i64
}

module hll = mk_hyperloglog_plusplus i64key

local
def both f (a, b) = (f a, f b)

local
def bimap f g (a, b) = (f a, g b)

local
def swap 't (a, b) : (t, t) = (b, a)

local
def hash (a: (u64, u64)) (b: (u64, u64)) (x: u64) : u64 =
  let y_mul_lo = a.0 * x
  in u64.mul_hi a.1 x + b.1 + u64.bool (y_mul_lo < b.0)

local
def find_by_vector [n] [u]
                   (none: i64)
                   (parents: *[n]i64)
                   (hs: [u]i64) : *([n]i64, [u]i64) =
  ( parents
  , map (\h ->
           loop h
           while parents[h] != none do
             parents[h])
        hs
  )

local
def find_by_vector' [n] [u]
                    (none: i64)
                    (parents: [n]i64)
                    (hs: [u]i64) : *[u]i64 =
  map (\h ->
         loop h
         while parents[h] != none do
           parents[h])
      hs

local
def compression_step [m] [n]
                     (none: i64)
                     (is: [m]i64)
                     (parents: *[n]i64)
                     (ps: [m]i64) : (*[n]i64, [m]i64) =
  let f h =
    if parents[h] == none
    then h
    else if parents[parents[h]] == none
    then parents[h]
    else parents[parents[h]]
  let ps' = map f ps
  let new_parents = scatter parents is ps'
  in (new_parents, ps')

local
def compression [m] [n]
                (none: i64)
                (parents: *[n]i64)
                (is: [m]i64) : (*[n]i64, [m]i64) =
  let ps = is
  let (new_parents, ps) =
    loop (parents, ps)
    for _i < 64 - i64.clz m do
      compression_step none is parents ps
  in (new_parents, ps)

module unionfind : unionfind = {
  type handle = i64

  type unionfind [n] = {parents: [n]handle}

  def to_i64 [n] (_: unionfind [n]) (h: handle) : i64 =
    assert (0 <= h && h < n) h

  def from_i64 [n] (_: unionfind [n]) (i: i64) : handle =
    assert (0 <= i && i < n) i

  def none : handle = i64.highest

  def handles [n] (_: unionfind [n]) : *[n]handle =
    iota n

  def create (n: i64) : *unionfind [n] =
    {parents = rep none}

  def find [n] [u]
           ({parents}: *unionfind [n])
           (hs: [u]handle) : *(unionfind [n], [u]handle) =
    let (new_parents, ps) = find_by_vector none parents hs
    in ({parents = new_parents}, ps)

  def find' [n] [u]
            (uf: unionfind [n])
            (hs: [u]handle) : *[u]handle =
    find_by_vector' none uf.parents hs

  def order [n] [u]
            (parents: *[n]handle)
            (eqs: [u](handle, handle)) : ( *[n]handle
                                         , [u](handle, handle)
                                         ) =
    let eqs_elems = unzip eqs |> uncurry (++)
    let (new_parents, new_eqs_elems) = find_by_vector none parents eqs_elems
    let eqs = split new_eqs_elems |> uncurry zip
    in (new_parents, eqs)

  def maximal_union [n] [u]
                    (parents: *[n]handle)
                    (eqs: [u](handle, handle)) : ?[m].(*[n]handle, [m](handle, handle)) =
    let (vs, us) = unzip eqs
    let unique_vs = hll.insert () (hll.create 10) vs |> hll.count
    let unique_us = hll.insert () (hll.create 10) us |> hll.count
    let (vs, us) = if unique_vs < unique_us then (us, vs) else (vs, us)
    let parents = reduce_by_index parents i64.min none vs us
    let (eqs, done) =
      copy (partition (\(i, p) -> parents[i] != p) eqs)
    let parents = compression none parents (map (.0) done) |> (.0)
    in (parents, eqs)

  def union [n] [u]
            ({parents}: *unionfind [n])
            (eqs: [u](handle, handle)) : *unionfind [n] =
    let (parents, _) =
      loop (parents, eqs)
      while length eqs != 0 do
        let (parents, eqs) = order parents eqs
        let eqs =
          map (\(v, u) -> if v < u then (v, u) else (u, v)) eqs
          |> filter (\(v, u) -> v != u)
        let (parents, eqs) = maximal_union parents eqs
        in (parents, eqs)
    in {parents}
}

module unionfind_by_size : unionfind = {
  type handle = i64

  type unionfind [n] =
    { parents: [n]handle
    , sizes: [n]i64
    , temporary_indices: [n]i64
    }

  def to_i64 [n] (_: unionfind [n]) (h: handle) : i64 =
    assert (0 <= h && h < n) h

  def from_i64 [n] (_: unionfind [n]) (i: i64) : handle =
    assert (0 <= i && i < n) i

  def none : handle = i64.highest

  def handles [n] (_: unionfind [n]) : *[n]handle =
    iota n

  def create (n: i64) : *unionfind [n] =
    { parents = rep none
    , sizes = rep 1
    , temporary_indices = rep none
    }

  def find [n] [u]
           ({parents, sizes, temporary_indices}: *unionfind [n])
           (hs: [u]handle) : *(unionfind [n], [u]handle) =
    let (new_parents, ps) = find_by_vector none parents hs
    in ({parents = new_parents, sizes, temporary_indices}, ps)

  def find' [n] [u]
            (uf: unionfind [n])
            (hs: [u]handle) : *[u]handle =
    find_by_vector' none uf.parents hs

  def maximal_union [n] [u]
                    (parents: *[n]handle)
                    (sizes: *[n]i64)
                    (temporary_indices: *[n]i64)
                    (eqs: [u](handle, handle)) : ?[m].( *[n]handle
                                                      , *[n]i64
                                                      , *[n]i64
                                                      , [m](handle, handle)
                                                      ) =
    let lefts = map (.0) eqs
    let eq_is = indices eqs
    let temporary_indices =
      reduce_by_index temporary_indices i64.min i64.highest lefts eq_is
    let (done, eqs) =
      zip (indices eqs) eqs
      |> partition (\(i, (l, _)) -> i == temporary_indices[l])
      |> bimap (map (.1)) (map (.1))
    let (is, ps) = unzip done
    let parents = scatter parents is ps
    let (new_parents, new_ps) = compression none parents is
    let children_sizes = map (\i -> sizes[i]) is
    let new_sizes = reduce_by_index sizes (+) 0 new_ps children_sizes
    let new_eqs = copy eqs
    let new_temporary_indices = scatter temporary_indices lefts (rep i64.highest)
    in (new_parents, new_sizes, new_temporary_indices, new_eqs)

  def order [n] [u]
            (parents: *[n]handle)
            (sizes: *[n]i64)
            (eqs: [u](handle, handle)) : ?[m].( *[n]handle
                                              , *[n]i64
                                              , [m](handle, handle)
                                              ) =
    let eqs_elems = unzip eqs |> uncurry (++)
    let (new_parents, new_eqs_elems) = find_by_vector none parents eqs_elems
    let (_, value_eqs, rank_eqs) =
      split new_eqs_elems
      |> uncurry zip
      |> map (\(v, u) ->
                if sizes[v] == sizes[u]
                then if v < u then (v, u) else (u, v)
                else if sizes[v] < sizes[u] then (v, u) else (u, v))
      |> partition2 (uncurry (==)) (\(v, u) -> sizes[v] == sizes[u])
    let (vs, us) = unzip value_eqs
    let unique_vs = hll.insert () (hll.create 10) vs |> hll.count
    let unique_us = hll.insert () (hll.create 10) us |> hll.count
    let value_eqs = if unique_vs < unique_us then zip us vs else zip vs us
    let eqs = value_eqs ++ rank_eqs
    in (new_parents, sizes, eqs)

  def union [n] [u]
            ({parents, sizes, temporary_indices}: *unionfind [n])
            (eqs: [u](handle, handle)) : *unionfind [n] =
    let (new_parents, new_sizes, new_temporary_indices, _) =
      loop (parents, sizes, temporary_indices, eqs)
      while not (null eqs) do
        let (parents, sizes, eqs) = order parents sizes eqs
        let (parents, sizes, temporary_indices, eqs) =
          maximal_union parents sizes temporary_indices eqs
        in (parents, sizes, temporary_indices, eqs)
    in { parents = new_parents
       , sizes = new_sizes
       , temporary_indices = new_temporary_indices
       }
}

module unionfind_by_rank : unionfind = {
  type handle = i64

  type unionfind [n] =
    { parents: [n]handle
    , ranks: [n]u8
    }

  def to_i64 [n] (_: unionfind [n]) (h: handle) : i64 =
    assert (0 <= h && h < n) h

  def from_i64 [n] (_: unionfind [n]) (i: i64) : handle =
    assert (0 <= i && i < n) i

  def none : handle = i64.highest

  def handles [n] (_: unionfind [n]) : *[n]handle =
    iota n

  def create (n: i64) : *unionfind [n] =
    { parents = rep none
    , ranks = rep 0
    }

  def find [n] [u]
           ({parents, ranks}: *unionfind [n])
           (hs: [u]handle) : *(unionfind [n], [u]handle) =
    let (new_parents, ps) = find_by_vector none parents hs
    in ({parents = new_parents, ranks}, ps)

  def find' [n] [u]
            (uf: unionfind [n])
            (hs: [u]handle) : *[u]handle =
    find_by_vector' none uf.parents hs

  def maximal_union [n] [u]
                    (parents: *[n]handle)
                    (ranks: *[n]u8)
                    (eqs: [u](handle, handle)) : ?[m].( *[n]handle
                                                      , *[n]u8
                                                      , [m](handle, handle)
                                                      ) =
    let (ls, rs) = unzip eqs
    let parents = reduce_by_index parents i64.min none ls rs
    let (new_eqs, done) =
      copy (partition (\(i, p) -> parents[i] != p) eqs)
    let is = map (.0) done
    let (new_parents, new_ps) = compression none parents is
    let new_ranks_done =
      copy
      <| map2 (\l p ->
                 u8.bool (ranks[l] u8.== ranks[p]) + ranks[p])
              is
              new_ps
    let new_ranks = reduce_by_index ranks u8.max 0 new_ps new_ranks_done
    in (new_parents, new_ranks, new_eqs)

  def order [n] [u]
            (parents: *[n]handle)
            (ranks: *[n]u8)
            (eqs: [u](handle, handle)) : ?[m].( *[n]handle
                                              , *[n]u8
                                              , [m](handle, handle)
                                              ) =
    let eqs_elems = unzip eqs |> uncurry (++)
    let (new_parents, new_eqs_elems) = find_by_vector none parents eqs_elems
    let (_, value_eqs, rank_eqs) =
      split new_eqs_elems
      |> uncurry zip
      |> map (\(v, u) ->
                if ranks[v] == ranks[u]
                then if v < u then (v, u) else (u, v)
                else if ranks[v] < ranks[u] then (v, u) else (u, v))
      |> partition2 (uncurry (==)) (\(v, u) -> ranks[v] == ranks[u])
    let (vs, us) = unzip value_eqs
    let unique_vs = hll.insert () (hll.create 10) vs |> hll.count
    let unique_us = hll.insert () (hll.create 10) us |> hll.count
    let value_eqs = if unique_vs < unique_us then zip us vs else zip vs us
    let eqs = value_eqs ++ rank_eqs
    in (new_parents, ranks, eqs)

  def union [n] [u]
            ({parents, ranks}: *unionfind [n])
            (eqs: [u](handle, handle)) : *unionfind [n] =
    let (new_parents, new_ranks, _) =
      loop (parents, ranks, eqs)
      while not (null eqs) do
        let (parents, ranks, eqs) = order parents ranks eqs
        let (parents, ranks, eqs) = maximal_union parents ranks eqs
        in (parents, ranks, eqs)
    in { parents = new_parents
       , ranks = new_ranks
       }
}

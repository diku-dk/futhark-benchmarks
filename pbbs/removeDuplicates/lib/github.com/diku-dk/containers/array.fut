-- | Functions on pure arrays.
--
-- This module contains modules and functions for manipulating arrays. Which
-- might be something like deduplication or a generalized reduction by a key.

import "../sorts/radix_sort"
import "../segmented/segmented"
import "hashkey"
import "opt"

local
-- | Fairly simple functions on arrays. Implemented by the module `array`@term.
module type array = {
  -- | True if the provided arrays have the same size and the same elements as
  -- determined by the provided operator.
  val eq [n] [m] 't : (eq: t -> t -> bool) -> [n]t -> [m]t -> bool

  -- | Lexicographical less-or-equal comparison of two arrays.
  val le [n] [m] 't : (lte: t -> t -> bool) -> [n]t -> [m]t -> bool
}

module array : array = {
  -- | Compare two arrays for equality using a provided operator.
  def eq [n] [m] 't ((==): t -> t -> bool) (x: [n]t) (y: [m]t) =
    n i64.== m
    && (loop (ok, i) = (true, 0)
        while ok && i < n do
          (ok && (x[i] == y[i]), i + 1)).0

  -- Lexicographical comparison of two arrays. Returns true if `a <= b`.
  def le [n] [m] 't ((<=): t -> t -> bool) (a: [n]t) (b: [m]t) : bool =
    let minlen = i64.min n m
    let (<) = \x y -> x <= y && !(y <= x)
    let cmp =
      map2 (\x y : opt bool ->
              if x < y
              then #some true
              else if y < x
              then #some false
              else #none)
           (take minlen a)
           (take minlen b)
    in match first_some cmp
       case #some res -> res
       case #none -> n i64.<= m
}

-- | A module type for functions on arrays of keys. Use `mk_array_key`@term
-- to produce a module that implements this module type, given a random number
-- generator.
module type array_key = {
  -- | The key type.
  type key

  -- | The context type.
  type ctx

  -- | The random number generator.
  type rng

  -- | Reduce by key works like Futharks `reduce_by_index` but instead of the
  -- need to make every value correspond to an index in some array it can
  -- instead correspond to a key. Here an array of `n` key `k` and value `v`
  -- pairs are given as an array. And every value with the same key will be
  -- reduced with an associative and commutative operator `op`, futhermore an
  -- neutral element `ne` must be given.
  val reduce_by_key [n] 'v :
    ctx
    -> rng
    -> (v -> v -> v)
    -> v
    -> [n](key, v)
    -> ?[m].(rng, [m](key, v))

  -- | Removes duplicate elements from an array as defined by the equality
  -- relation of the key. This implementation works much like `reduce_by_key`
  -- but saves some steps which makes it faster.
  val dedup [n] : ctx -> rng -> [n]key -> ?[m].(rng, [m]key)
}

module mk_array_key_params
  (I: integral)
  (U: integral)
  (K: hashkey with hash = U.t)
  : array_key
    with rng = K.rng
    with key = K.key
    with ctx = K.ctx = {
  module key = K
  type int = I.t
  type uint = U.t
  type rng = K.rng
  type key = key.key
  type~ ctx = K.ctx

  def to_int : uint -> int =
    I.i64 <-< U.to_i64

  def dedup [n] (ctx: ctx) (r: rng) (arr: [n]key) : ?[m].(rng, [m]key) =
    if n == 0
    then (r, [])
    else let keq a b = (ctx, a) key.== (ctx, b)
         let add2 (a0, a1) (b0, b1) = (a0 + b0, a1 + b1)
         let dest = replicate n arr[0]
         let (uniques, _, final_size, final_rng) =
           loop ( uniques: *[n]key
                , elems: *[]key
                , old_size
                , old_rng
                ) =
                  (dest, arr, 0, r)
           while length elems != 0 do
             let (new_rng, consts) = K.rand old_rng
             let size = i64.max 4096 (length elems)
             let alloc_size = U.i64 (size + size / 2)
             let h = to_int <-< (U.%% alloc_size) <-< key.hash ctx consts
             let hashes = map h elems
             let collision_idxs =
               hist I.min
                    I.highest
                    (U.to_i64 alloc_size)
                    (map I.to_i64 hashes)
                    (map I.i64 (indices hashes))
             let flags =
               zip3 (indices elems) hashes elems
               |> map (\(i, h', v) ->
                         #[unsafe]
                         let idx = collision_idxs[I.to_i64 h']
                         let idx' = I.to_i64 idx
                         let is_chosen = idx' == i
                         let is_duplicate =
                           idx I.!= I.highest && not (elems[idx'] `keq` v)
                         in ( is_chosen
                            , is_duplicate
                            ))
             let scan_res =
               flags
               |> map (\(a, b) -> (i64.bool a, i64.bool b))
               |> scan add2 (0, 0)
             let count = scatter [(0, 0)] (map (\i -> if i == length scan_res - 1 then 0 else -1) (indices scan_res)) scan_res
             let (unique_size, continue_size) = count[0]
             let (unique_is, continue_is) =
               scan_res
               |> map2 (\(f0, f1) (i0, i1) ->
                          if f0
                          then (old_size + i0 - 1, -1)
                          else if f1
                          then (-1, i1 - 1)
                          else (-1, -1))
                       flags
               |> unzip
             let new_uniques = scatter uniques unique_is elems
             let new_elems = scatter (#[scratch] copy elems) continue_is elems
             in ( new_uniques
                , new_elems[:continue_size]
                , old_size + unique_size
                , new_rng
                )
         in (final_rng, take final_size uniques)

  def reduce_by_key [n] 'v
                    (ctx: ctx)
                    (r: rng)
                    (op: v -> v -> v)
                    (ne: v)
                    (arr: [n](key, v)) : (rng, [](key, v)) =
    if n == 0
    then (r, [])
    else let keq a b = (ctx, a) key.== (ctx, b)
         let add2 (a0, a1) (b0, b1) = (a0 + b0, a1 + b1)
         let reduced_keys_dest = replicate n arr[0].0
         let reduced_values_dest = replicate n ne
         let ( not_reduced_keys_init
             , not_reduced_values_init
             ) =
           unzip arr
         let (reduced_keys, reduced_values, _, _, final_size, final_rng) =
           -- Expected number of iterations is O(log n).
           loop ( reduced_keys: *[n]key
                , reduced_values: *[n]v
                , not_reduced_keys: *[]key
                , not_reduced_values: *[]v
                , old_size
                , old_rng
                ) =
                  ( reduced_keys_dest
                  , reduced_values_dest
                  , copy not_reduced_keys_init
                  , copy not_reduced_values_init
                  , 0
                  , r
                  )
           while length not_reduced_keys != 0 do
             let (new_rng, consts) = K.rand old_rng
             let size = i64.max 4096 (length not_reduced_keys)
             let alloc_size = U.i64 (size + size / 2)
             let h = to_int <-< (U.%% alloc_size) <-< key.hash ctx consts
             let hashes = map h not_reduced_keys
             let collision_idxs =
               hist I.min
                    I.highest
                    (U.to_i64 alloc_size)
                    (map I.to_i64 hashes)
                    (map I.i64 (indices hashes))
             let (flags, reduce_hashes) =
               zip3 (indices not_reduced_keys) hashes not_reduced_keys
               |> map (\(i, h', k) ->
                         #[unsafe]
                         let idx = collision_idxs[I.to_i64 h']
                         let idx' = I.to_i64 idx
                         let is_chosen = idx' == i
                         let is_duplicate =
                           idx I.!= I.highest && not (not_reduced_keys[idx'] `keq` k)
                         let is_equal =
                           idx I.!= I.highest && (not_reduced_keys[idx'] `keq` k)
                         let i = if is_equal then h' else I.i64 (-1)
                         in ((is_chosen, is_duplicate), i))
               |> unzip
             let reduced_by_hash =
               hist op
                    ne
                    (U.to_i64 alloc_size)
                    (map I.to_i64 reduce_hashes)
                    not_reduced_values
             let temp_reduced_values =
               map2 (\(f, _) h ->
                       #[unsafe]
                       if f
                       then reduced_by_hash[I.to_i64 h]
                       else ne)
                    flags
                    hashes
             let scan_res =
               flags
               |> map (\(a, b) -> (i64.bool a, i64.bool b))
               |> scan add2 (0, 0)
             let count = scatter [(0, 0)] (map (\i -> if i == length scan_res - 1 then 0 else -1) (indices scan_res)) scan_res
             let (unique_size, continue_size) = count[0]
             let (unique_is, continue_is) =
               scan_res
               |> map2 (\(f0, f1) (i0, i1) ->
                          if f0
                          then (old_size + i0 - 1, -1)
                          else if f1
                          then (-1, i1 - 1)
                          else (-1, -1))
                       flags
               |> unzip
             let new_reduced_keys = scatter reduced_keys unique_is not_reduced_keys
             let new_reduced_values = scatter reduced_values unique_is temp_reduced_values
             let new_not_reduced_keys = scatter (#[scratch] copy not_reduced_keys) continue_is not_reduced_keys
             let new_not_reduced_values = scatter (#[scratch] copy not_reduced_values) continue_is not_reduced_values
             in ( new_reduced_keys
                , new_reduced_values
                , new_not_reduced_keys[:continue_size]
                , new_not_reduced_values[:continue_size]
                , old_size + unique_size
                , new_rng
                )
         in (final_rng, take final_size (zip reduced_keys reduced_values))
}

module mk_array_key = mk_array_key_params i64 u64

-- | Array-based key-value maps.
--
-- Requires an ordering on the elements to provide relatively efficient lookups.

import "../sorts/merge_sort"
import "../segmented/segmented"
import "opt"
import "array"
import "map"
import "ordkey"

local
def binary_search [n] 't (eq: t -> t -> bool) (lte: t -> t -> bool) (xs: [n]t) (x: t) : i64 =
  let (l, _) =
    loop (l, r) = (0, n - 1)
    while l < r do
      let t = l + (r - l) / 2
      in if x `lte` xs[t]
         then (l, t)
         else (t + 1, r)
  in if l >= 0 && l < n && (xs[l] `eq` x) then l else -1

-- | A map that uses a sorted array to represent the mapping.
module mk_arraymap (K: ordkey) : map with key = K.key with ctx = K.ctx = {
  type key = K.key
  type~ ctx = K.ctx

  type~ map [n] 'a =
    { ctx: ctx
    , keys: [n]key
    , vals: [n]a
    }

  def neq lte x y = if x `lte` y then !(y `lte` x) else true

  def pack eq xs =
    zip3 (indices xs) xs (rotate (-1) xs)
    |> filter (\(i, x, y) -> i == 0 || !(eq x y))
    |> map (.1)

  def from_array [u] 'v (ctx: ctx) (kvs: [u](key, v)) : ?[n].map [n] v =
    let (keys, vals) =
      kvs
      |> merge_sort_by_key (.0) (\x y -> (ctx, x) K.<= (ctx, y))
      |> pack (\x y -> (ctx, x.0) K.== (ctx, y.0))
      |> unzip
    in {keys, vals, ctx}

  def from_array_rep [u] 'v (ctx: ctx) (keys: [u]key) (v: v) : ?[n].map [n] v =
    let lte x y = (ctx, x) K.<= (ctx, y)
    let keys =
      keys
      |> merge_sort lte
      |> pack lte
    in {keys, vals = map (const v) keys, ctx}

  def from_array_hist [u] 'v (ctx: ctx) (op: v -> v -> v) (ne: v) (kvs: [u](key, v)) : ?[n].map [n] v =
    let (keys, vals) =
      kvs
      |> merge_sort_by_key (.0) (\x y -> (ctx, x) K.<= (ctx, y))
      |> (\kvs ->
            let (keys, vals) = unzip kvs
            let flags =
              map3 (\i x y -> i == 0 || !((ctx, x) K.== (ctx, y)))
                   (indices kvs)
                   keys
                   (rotate (-1) keys)
            let [m] (keys_uniq: [m]key) = zip keys flags |> filter (.1) |> map (.0)
            in zip keys_uniq (sized m (segmented_reduce op ne flags vals)))
      |> unzip
    in {keys, vals, ctx}

  def from_array_nodup [u] 'v (ctx: ctx) (kvs: [u](key, v)) : ?[n].map [n] v =
    let (keys, vals) =
      kvs
      |> merge_sort_by_key (.0) (\x y -> (ctx, x) K.<= (ctx, y))
      |> unzip
    in {keys, vals, ctx}

  def from_array_rep_nodup [n] 'v (ctx: ctx) (keys: [n]key) (v: v) : ?[n].map [n] v =
    let keys =
      keys
      |> merge_sort (\x y -> (ctx, x) K.<= (ctx, y))
    in {keys, vals = map (const v) keys, ctx}

  def lookup_index [n] 'a (ctx: ctx) (k: key) (m: map [n] a) : i64 =
    binary_search (\x y -> (m.ctx, x) K.== (ctx, y))
                  (\x y -> (m.ctx, x) K.<= (ctx, y))
                  m.keys
                  k

  def lookup [n] 'a (ctx: ctx) (k: key) (m: map [n] a) : opt a =
    match lookup_index ctx k m
    case -1 -> #none
    case i -> #some m.vals[i]

  def member [n] 'a (ctx: ctx) (k: key) (m: map [n] a) : bool =
    -1 != lookup_index ctx k m

  def not_member ctx k xs =
    !(member ctx k xs)

  def adjust [n] [u] 'v (op: v -> v -> v) (ne: v) (m: map [n] v) (kvs: [u](key, v)) : map [n] v =
    let is = map (\k -> lookup_index m.ctx k m) (map (.0) kvs)
    in m with vals = reduce_by_index (copy m.vals) op ne is (map (.1) kvs)

  def to_array 'a (m: map [] a) =
    zip m.keys m.vals

  def update [n] [u] 'v (m: map [n] v) (kvs: [u](key, v)) : map [n] v =
    let (ks, vs) = unzip kvs
    let js = map (\k -> lookup_index m.ctx k m) ks
    let is = hist i64.min i64.highest n js (iota u)
    let new_vals = map2 (\i v -> if i != i64.highest then vs[i] else v) is m.vals
    in m with vals = new_vals

  def size [n] 'a (_: map [n] a) = n

  def context [n] 'a (m: map [n] a) = m.ctx

  def insert [n] [u] 'v (ctx: ctx) (m: map [n] v) (kvs: [u](key, v)) : ?[m].map [m] v =
    from_array ctx (kvs ++ to_array m)

  def insert_with [n] [u] 'v
                  (ctx: ctx)
                  (op: v -> v -> v)
                  (ne: v)
                  (m: map [n] v)
                  (kvs: [u](key, v)) : ?[m].map [m] v =
    from_array_hist ctx op ne (to_array m ++ kvs)

  def reduce [n] 'v
             (op: v -> v -> v)
             (ne: v)
             (m: map [n] v) : v =
    reduce_comm op ne m.vals

  def map [n] 'a 'b (g: a -> b) ({ctx, keys, vals}: map [n] a) : map [n] b =
    {ctx, keys, vals = map g vals}

  def map_with_key [n] 'a 'b (g: key -> a -> b) ({ctx, keys, vals}: map [n] a) : map [n] b =
    let vals = map2 g keys vals
    in {ctx, keys, vals}
}

-- | Eytzinger tree-based key-value maps.
--
-- Requires an ordering on the elements to provide relatively efficient lookups.

import "../sorts/merge_sort"
import "../segmented/segmented"
import "opt"
import "array"
import "map"
import "ordkey"

module type eytzinger_unlifted = {
  type key

  type ctx

  type map 'ctx [n] [f] 'v

  val member [n] [f] 'v : ctx -> key -> map ctx [n] [f] v -> bool

  val not_member [n] [f] 'v : ctx -> key -> map ctx [n] [f] v -> bool

  val lookup [n] [f] 'v : ctx -> key -> map ctx [n] [f] v -> opt v

  val from_array [u] 'v :
    ctx -> [u](key, v) -> ?[n][f].map ctx [n] [f] v

  val from_array_rep [u] 'v :
    ctx
    -> [u]key
    -> v
    -> ?[n][f].map ctx [n] [f] v

  val from_array_hist [u] 'v :
    ctx
    -> (v -> v -> v)
    -> v
    -> [u](key, v)
    -> ?[n][f].map ctx [n] [f] v

  val from_array_nodup [n] 'v :
    ctx -> [n](key, v) -> ?[f].map ctx [n] [f] v

  val from_array_rep_nodup [n] 'v :
    ctx
    -> [n]key
    -> v
    -> ?[f].map ctx [n] [f] v

  val adjust [n] [f] [u] 'v :
    (v -> v -> v)
    -> v
    -> map ctx [n] [f] v
    -> [u](key, v)
    -> map ctx [n] [f] v

  val map [n] [f] 'v 't :
    (g: v -> t) -> map ctx [n] [f] v -> map ctx [n] [f] t

  val map_with_key [n] [f] 'v 't :
    (g: key -> v -> t)
    -> map ctx [n] [f] v
    -> map ctx [n] [f] t

  val to_array [n] [f] 'v : map ctx [n] [f] v -> [n](key, v)

  val update [n] [f] [u] 'v :
    map ctx [n] [f] v
    -> [u](key, v)
    -> map ctx [n] [f] v

  val size [n] [f] 'v : map ctx [n] [f] v -> i64

  val context [n] [f] 'v : map ctx [n] [f] v -> ctx

  val insert [n] [f] [u] 'v :
    ctx
    -> map ctx [n] [f] v
    -> [u](key, v)
    -> ?[n'][f'].map ctx [n'] [f'] v

  val insert_with [n] [f] [u] 'v :
    ctx
    -> (v -> v -> v)
    -> v
    -> map ctx [n] [f] v
    -> [u](key, v)
    -> ?[n'][f'].map ctx [n'] [f'] v

  val reduce [n] [f] 'v :
    (v -> v -> v)
    -> v
    -> map ctx [n] [f] v
    -> v
}

-- | A map that uses a eytzinger tree to represent the mapping.
module mk_eytzinger_unlifted (K: ordkey) : eytzinger_unlifted with key = K.key with ctx = K.ctx = {
  type key = K.key
  type~ ctx = K.ctx

  type map 'ctx [n] [f] 'a =
    { ctx: ctx
    , offsets: [f]i64
    , lookup_keys: [f]key
    , vals: [n]a
    , keys: [n]key
    }

  local
  def empty 'v ctx : ?[f].map ctx [0] [f] v =
    {ctx, keys = [], lookup_keys = [], vals = [], offsets = []}

  def neq lte x y = if x `lte` y then !(y `lte` x) else true

  def pack eq xs =
    zip3 (indices xs) xs (rotate (-1) xs)
    |> filter (\(i, x, y) -> i == 0 || !(eq x y))
    |> map (.1)

  local
  def log2 x = 63 - i64.clz x

  local
  def eytzinger_index (n: i64) (i: i64) =
    let lvl = log2 (i + 1)
    let offset = i64.i32 (1 << (log2 n - lvl))
    let k = i64.i32 ((1 << lvl) - 1)
    in offset + (i - k) * offset * 2 - 1

  local
  def eytzinger [n] 't (xs: [n]t) : ?[m].[m]t =
    let m = 2 ** (i64.num_bits - i64.clz n |> i64.i32) - 1
    let dest = if n == 0 then [] else replicate m xs[n - 1]
    let xs' = scatter dest (indices xs) xs
    let f i = xs'[eytzinger_index m i]
    in tabulate m f

  local
  def ffs x = i64.ctz x + 1

  local
  def lookup_flat_index [n] [f] 'v (ctx: ctx) (x: key) (m: map ctx [n] [f] v) : i64 =
    let k =
      loop k = 1
      while k <= f do
        if (ctx, x) K.<= (m.ctx, m.lookup_keys[k - 1])
        then 2 * k
        else 2 * k + 1
    let i = (k >> i64.i32 (ffs (!k))) - 1
    in if (0 <= i && i < f && (ctx, x) K.== (m.ctx, m.lookup_keys[i]))
       then i
       else -1

  local
  def lookup_index [n] [f] 'v (ctx: ctx) (x: key) (m: map ctx [n] [f] v) : i64 =
    let i = lookup_flat_index ctx x m
    in if i != -1
       then m.offsets[i]
       else -1

  def from_array_rep [u] 'v (ctx: ctx) (keys: [u]key) (v: v) : ?[n][f].map ctx [n] [f] v =
    let lte x y = (ctx, x) K.<= (ctx, y)
    let keys =
      keys
      |> merge_sort lte
      |> pack lte
    let (lookup_keys, offsets) =
      keys
      |> (\ks -> zip ks (indices ks))
      |> eytzinger
      |> unzip
    in {keys, lookup_keys, vals = map (const v) keys, ctx, offsets}

  def from_array [u] 'v (ctx: ctx) (kvs: [u](key, v)) : ?[n][f].map ctx [n] [f] v =
    let lte x y = (ctx, x) K.<= (ctx, y)
    let eq x y = (ctx, x.0) K.== (ctx, y.0)
    let (keys, vals) =
      kvs
      |> merge_sort_by_key (.0) lte
      |> pack eq
      |> unzip
    let (lookup_keys, offsets) =
      keys
      |> (\ks -> zip ks (indices ks))
      |> eytzinger
      |> unzip
    in {keys, lookup_keys, vals, ctx, offsets}

  def from_array_hist [u] 'v (ctx: ctx) (op: v -> v -> v) (ne: v) (kvs: [u](key, v)) : ?[n][f].map ctx [n] [f] v =
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
    let (lookup_keys, offsets) =
      keys
      |> (\ks -> zip ks (indices ks))
      |> eytzinger
      |> unzip
    in {keys, lookup_keys, vals, ctx, offsets}

  def from_array_nodup [n] 'v (ctx: ctx) (kvs: [n](key, v)) : ?[f].map ctx [n] [f] v =
    let lte x y = (ctx, x) K.<= (ctx, y)
    let (keys, vals) =
      kvs
      |> merge_sort_by_key (.0) lte
      |> unzip
    let (lookup_keys, offsets) =
      keys
      |> (\ks -> zip ks (indices ks))
      |> eytzinger
      |> unzip
    in {keys, lookup_keys, vals, ctx, offsets}

  def from_array_rep_nodup [n] 'v (ctx: ctx) (keys: [n]key) (v: v) : ?[f].map ctx [n] [f] v =
    let lte x y = (ctx, x) K.<= (ctx, y)
    let keys =
      keys
      |> merge_sort lte
    let (lookup_keys, offsets) =
      keys
      |> (\ks -> zip ks (indices ks))
      |> eytzinger
      |> unzip
    in {keys, lookup_keys, vals = map (const v) keys, ctx, offsets}

  def lookup [n] [f] 'a (ctx: ctx) (k: key) (m: map ctx [n] [f] a) : opt a =
    match lookup_index ctx k m
    case -1 -> #none
    case i -> #some m.vals[i]

  def member [n] [f] 'a (ctx: ctx) (k: key) (m: map ctx [n] [f] a) : bool =
    -1 != lookup_flat_index ctx k m

  def not_member ctx k xs =
    !(member ctx k xs)

  def adjust [n] [f] [u] 'v (op: v -> v -> v) (ne: v) (m: map ctx [n] [f] v) (kvs: [u](key, v)) : map ctx [n] [f] v =
    let is = map (\k -> lookup_index m.ctx k m) (map (.0) kvs)
    in m with vals = reduce_by_index (copy m.vals) op ne is (map (.1) kvs)

  def to_array [n] [f] 'a (m: map ctx [n] [f] a) =
    zip m.keys m.vals

  def update [n] [f] [u] 'v (m: map ctx [n] [f] v) (kvs: [u](key, v)) : map ctx [n] [f] v =
    let (ks, vs) = unzip kvs
    let js = map (\k -> lookup_index m.ctx k m) ks
    let is = hist i64.min i64.highest n js (iota u)
    let new_vals = map2 (\i v -> if i != i64.highest then vs[i] else v) is m.vals
    in m with vals = new_vals

  def size [n] [f] 'a (_: map ctx [n] [f] a) = n

  def context [n] [f] 'a (m: map ctx [n] [f] a) = m.ctx

  def insert [n] [f] [u] 'v (ctx: ctx) (m: map ctx [n] [f] v) (kvs: [u](key, v)) : ?[m][k].map ctx [m] [k] v =
    from_array ctx (kvs ++ to_array m)

  def insert_with [n] [f] [u] 'v
                  (ctx: ctx)
                  (op: v -> v -> v)
                  (ne: v)
                  (m: map ctx [n] [f] v)
                  (kvs: [u](key, v)) : ?[m][k].map ctx [m] [k] v =
    from_array_hist ctx op ne (to_array m ++ kvs)

  def reduce [n] [f] 'v
             (op: v -> v -> v)
             (ne: v)
             (m: map ctx [n] [f] v) : v =
    reduce_comm op ne m.vals

  def map [n] [f] 'a 'b (g: a -> b) ({ctx, keys, vals, offsets, lookup_keys}: map ctx [n] [f] a) : map ctx [n] [f] b =
    {ctx, keys, vals = map g vals, offsets, lookup_keys}

  def map_with_key [n] [f] 'a 'b (g: key -> a -> b) ({ctx, keys, vals, offsets, lookup_keys}: map ctx [n] [f] a) : map ctx [n] [f] b =
    let vals = map2 g keys vals
    in {ctx, keys, vals, offsets, lookup_keys}
}

module mk_eytzinger (K: ordkey)
  : map
    with key = K.key
    with ctx = K.ctx = {
  module eytzinger = mk_eytzinger_unlifted K

  type key = eytzinger.key
  type ctx = eytzinger.ctx

  type~ map [n] 'v =
    ?[f].eytzinger.map ctx [n] [f] v

  def from_array [n] 'v (ctx: ctx) (key_values: [n](key, v)) : ?[m].map [m] v =
    eytzinger.from_array ctx key_values

  def from_array_rep [n] 'v (ctx: ctx) (keys: [n]key) (ne: v) : ?[m].map [m] v =
    eytzinger.from_array_rep ctx keys ne

  def adjust [n] [u] 'v
             (op: v -> v -> v)
             (ne: v)
             (hmap: map [n] v)
             (key_values: [u](key, v)) =
    eytzinger.adjust op ne hmap key_values

  def from_array_hist [u] 'v
                      (ctx: ctx)
                      (op: v -> v -> v)
                      (ne: v)
                      (key_values: [u](key, v)) : ?[n].map [n] v =
    eytzinger.from_array_hist ctx op ne key_values

  def from_array_nodup [n] 'v (ctx: ctx) (key_values: [n](key, v)) : map [n] v =
    eytzinger.from_array_nodup ctx key_values

  def from_array_rep_nodup [n] 'v (ctx: ctx) (keys: [n]key) (ne: v) : map [n] v =
    eytzinger.from_array_rep_nodup ctx keys ne

  def reduce [n] 'v
             (op: v -> v -> v)
             (ne: v)
             (m: map [n] v) : v =
    eytzinger.reduce op ne m

  def map [n] 'v 't (g: v -> t) (hmap: map [n] v) : map [n] t =
    eytzinger.map g hmap

  def map_with_key [n] 'v 't (g: key -> v -> t) (hmap: map [n] v) : map [n] t =
    eytzinger.map_with_key g hmap

  def update [n] [u] 'v (hmap: map [n] v) (key_values: [u](key, v)) =
    eytzinger.update hmap key_values

  def to_array [n] 'v (hmap: map [n] v) : [n](key, v) =
    eytzinger.to_array hmap

  def size [n] 'v (hmap: map [n] v) =
    eytzinger.size hmap

  def member [n] 'v (ctx: ctx) (k: key) (hmap: map [n] v) : bool =
    eytzinger.member ctx k hmap

  def not_member [n] 'v (ctx: ctx) (key: key) (hmap: map [n] v) : bool =
    eytzinger.not_member ctx key hmap

  def lookup [n] 'v (ctx: ctx) (k: key) (hmap: map [n] v) : opt v =
    eytzinger.lookup ctx k hmap

  def context hmap =
    eytzinger.context hmap

  def insert [n] [u] 'v
             (ctx: ctx)
             (hmap: map [n] v)
             (key_values: [u](key, v)) =
    eytzinger.insert ctx hmap key_values

  def insert_with [n] [u] 'v
                  (ctx: ctx)
                  (op: v -> v -> v)
                  (ne: v)
                  (hmap: map [n] v)
                  (key_values: [u](key, v)) =
    eytzinger.insert_with ctx op ne hmap key_values
}

-- | Static hashmaps.
--
-- Implementations of the `map`@mtype@"map" module type using hash-based data structures.

import "../segmented/segmented"
import "opt"
import "hash"
import "hashkey"
import "array"
import "map"

-- | A module type that more directly exposes the implementation details of the
-- hash table. The key used must have have a universal hash function.
module type two_level_hashmap = {
  -- | The key type.
  type key

  -- | The context type.
  type ctx

  -- | The hashmap definition.
  type map 'ctx [n] [f] [m] 'v

  -- | Check if a key is member of the hashmap.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val member [n] [f] [m] 'v : ctx -> key -> map ctx [n] [f] [m] v -> bool

  -- | Check if a key is not member of the hashmap
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val not_member [n] [f] [m] 'v : ctx -> key -> map ctx [n] [f] [m] v -> bool

  -- | Look up a value.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val lookup [n] [f] [m] 'v : ctx -> key -> map ctx [n] [f] [m] v -> opt v

  -- | Given a key-value array construct a hashmap.
  --
  -- **Expected Work:** *O(n + u)*
  --
  -- **Expected Span:** *O(log n)*
  val from_array [u] 'v :
    ctx -> [u](key, v) -> ?[n][f][m].map ctx [n] [f] [m] v

  -- | Create hashmap with default value.
  --
  -- **Expected Work:** *O(n)*
  --
  -- **Expected Span:** *O(log n)*
  val from_array_rep [u] 'v :
    ctx
    -> [u]key
    -> v
    -> ?[n][f][m].map ctx [n] [f] [m] v

  -- | Create hashmap where duplicates are reduced with an commutative
  -- and associative operation.
  --
  -- **Expected Work:** *O(n + u ✕ W(op))*
  --
  -- **Expected Span:** *O(log n)* (Assuming best case for hist)
  val from_array_hist [u] 'v :
    ctx
    -> (v -> v -> v)
    -> v
    -> [u](key, v)
    -> ?[n][f][m].map ctx [n] [f] [m] v

  -- | Given a key-value array construct a hashmap. If any keys are
  -- duplicates then the function call will never finish. It does less
  -- work than the safe variant.
  --
  -- **Expected Work:** *O(n + u)*
  --
  -- **Expected Span:** *O(log n)*
  val from_array_nodup [n] 'v :
    ctx -> [n](key, v) -> ?[f][m].map ctx [n] [f] [m] v

  -- | Create hashmap with default value. If any keys are duplicates
  -- then the function call will never finish. It does less work than
  -- the safe variant.
  --
  -- **Expected Work:** *O(n)*
  --
  -- **Expected Span:** *O(log n)*
  val from_array_rep_nodup [n] 'v :
    ctx
    -> [n]key
    -> v
    -> ?[f][m].map ctx [n] [f] [m] v

  -- | Combine key-value pairs into a map using the provided
  -- associative and commutative operation. Keys that are not present
  -- in the map is not added.
  --
  -- **Expected Work:** *O(n + u ✕ W(op))*
  --
  -- **Span:** *O(u)* in the worst case but O(1) in the best case.
  val adjust [n] [f] [m] [u] 'v :
    (v -> v -> v)
    -> v
    -> map ctx [n] [f] [m] v
    -> [u](key, v)
    -> map ctx [n] [f] [m] v

  -- | Map a function over the hashmap values.
  --
  -- **Expected Work:** *O(n ✕ W(g))*
  --
  -- **Span:** *O(S(g))*
  val map [n] [f] [m] 'v 't :
    (g: v -> t) -> map ctx [n] [f] [m] v -> map ctx [n] [f] [m] t

  -- | Map a function over the hashmap values.
  --
  -- **Expected Work:** *O(n ✕ W(g))*
  --
  -- **Span:** *O(S(g))*
  val map_with_key [n] [f] [m] 'v 't :
    (g: key -> v -> t)
    -> map ctx [n] [f] [m] v
    -> map ctx [n] [f] [m] t

  -- | Convert hashmap to an array of key-value pairs.
  --
  -- **Work:** *O(n)*
  --
  -- **Span:** *O(1)*
  val to_array [n] [f] [m] 'v : map ctx [n] [f] [m] v -> [n](key, v)

  -- | Updates the value of the hash map using the key with the
  -- smallest index. No new keys are added.
  --
  -- **Expected Work:** *O(n + u)*
  --
  -- **Span:** *O(u)* in the worst case but O(1) in the best case.
  val update [n] [f] [m] [u] 'v :
    map ctx [n] [f] [m] v
    -> [u](key, v)
    -> map ctx [n] [f] [m] v

  -- | The number of elements in the hashmap.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val size [n] [f] [m] 'v : map ctx [n] [f] [m] v -> i64

  -- | Gets the context of the hashmap.
  --
  -- **Work:** *O(1)*
  --
  -- **Span:** *O(1)*
  val context [n] [f] [m] 'v : map ctx [n] [f] [m] v -> ctx

  -- | Insert new key-value pairs into a hashmap. If a key already
  -- exists in the hashmap, the new value will overwrite the old one.
  --
  -- **Expected Work:** *O(n + u)*
  --
  -- **Expected Span:** *O(log (n + u))*
  val insert [n] [f] [m] [u] 'v :
    ctx
    -> map ctx [n] [f] [m] v
    -> [u](key, v)
    -> ?[n'][f'][m'].map ctx [n'] [f'] [m'] v

  -- | Insert new key-value pairs into a hashmap, combining values of
  -- duplicate keys using the provided associative and commutative
  -- operation.
  --
  -- **Expected Work:** *O(n' + (n + u) ✕ W(op))*
  --
  -- **Expected Span:** *O(log (n + u))* (Assuming best case for hist)
  val insert_with [n] [f] [m] [u] 'v :
    ctx
    -> (v -> v -> v)
    -> v
    -> map ctx [n] [f] [m] v
    -> [u](key, v)
    -> ?[n'][f'][m'].map ctx [n'] [f'] [m'] v

  -- | Reduce values into a single value by an associative and
  -- commutative operator.
  --
  -- **Work:** *O(n ✕ W(op))*
  --
  -- **Span:** *O(log(n) ✕ W(op))*
  val reduce [n] [f] [m] 'v :
    (v -> v -> v)
    -> v
    -> map ctx [n] [f] [m] v
    -> v
}

-- | This is an implementation of a static hash table using [two level
-- hashing](https://en.wikipedia.org/wiki/Double_hashing). The modules time
-- complexities assumes only unique keys but the modules does work with
-- duplicate keys. The key used must have a universal hash function.
module mk_two_level_hashmap
  (I: integral)
  (U: integral)
  (K: hashkey with hash = U.t)
  : two_level_hashmap
    with key = K.key
    with ctx = K.ctx = {
  module key = K
  type uint = U.t
  type int = I.t
  type rng = key.rng
  type key = key.key
  type const = key.const
  type~ ctx = key.ctx
  module array = mk_array_key_params I U K

  def neg_one = I.i64 (-1)
  def zero = I.i64 0
  def one = I.i64 1
  def two = I.i64 2
  def to_uint = U.i64 <-< I.to_i64
  def to_int = I.i64 <-< U.to_i64

  -- | The hashmap data type.
  type map 'ctx [n] [f] [m] 'v =
    { ctx: ctx
    , size: [n]()
    , keys: [f]key
    , values: [f]v
    , level_one_const: const
    , level_two_consts: [m]const
    , level_two: [n][3]int
    , filler: (key, i64)
    , rng: rng
    }

  local
  def empty 'v ctx : ?[f][m].map ctx [0] [f] [m] v =
    let r = key.rng_from_seed [123]
    let (r', c) = key.rand r
    in { ctx
       , size = []
       , keys = []
       , values = []
       , level_one_const = c
       , level_two_consts = []
       , level_two = []
       , filler = #[blank] [][0]
       , rng = r'
       }

  local
  def exscan [n] 'a (op: a -> a -> a) (ne: a) (as: [n]a) : (a, [n]a) =
    if length as == 0
    then (ne, as)
    else let res = scan op ne as |> rotate (-1)
         let l = copy res[0]
         let res[0] = ne
         in (l, res)

  local
  #[inline]
  def lookup_flat_index [n] [m]
                        (ctx: ctx)
                        (level_one_const: const)
                        (level_two_consts: [m]const)
                        (level_two: [n][3]int)
                        (flat_size: int)
                        (k: key) : int =
    if n == 0
    then neg_one
    else -- The offset to get the level two hash function.
         let o = U.((key.hash ctx level_one_const k) %% i64 n)
         let arr = level_two[U.to_i64 o]
         -- The offset into the flat representation
         let o' = to_uint arr[0]
         -- The size of the subarray.
         let s = to_uint arr[1]
         let cs = level_two_consts[I.to_i64 arr[2]]
         let y = U.((key.hash ctx cs k) %% max (i64 1) s)
         in I.(min (flat_size - one) (to_int U.(o' + y)))

  def member [n] [f] [m] 'v
             (ctx: ctx)
             (k: key)
             (hmap: map ctx [n] [f] [m] v) : bool =
    let i =
      lookup_flat_index ctx
                        hmap.level_one_const
                        hmap.level_two_consts
                        hmap.level_two
                        (I.i64 f)
                        k
    in if neg_one I.== i
       then false
       else let k' = hmap.keys[I.to_i64 i]
            in (hmap.ctx, k') key.== (ctx, k)

  def not_member [n] [f] [m] 'v
                 (ctx: ctx)
                 (key: key)
                 (hmap: map ctx [n] [f] [m] v) : bool =
    not (member ctx key hmap)

  local
  def lookup_index [n] [f] [m] 'v
                   (ctx: ctx)
                   (hmap: map ctx [n] [f] [m] v)
                   (k: key) : int =
    let i =
      lookup_flat_index ctx
                        hmap.level_one_const
                        hmap.level_two_consts
                        hmap.level_two
                        (I.i64 f)
                        k
    in if i I.== neg_one
       then neg_one
       else let k' = hmap.keys[I.to_i64 i]
            in if (hmap.ctx, k') key.== (ctx, k) then i else neg_one

  def lookup [n] [f] [m] 'v
             (ctx: ctx)
             (k: key)
             (hmap: map ctx [n] [f] [m] v) : opt v =
    let i =
      lookup_flat_index ctx
                        hmap.level_one_const
                        hmap.level_two_consts
                        hmap.level_two
                        (I.i64 f)
                        k
    in if i I.== neg_one
       then #none
       else let k' = hmap.keys[I.to_i64 i]
            let v = hmap.values[I.to_i64 i]
            in if (hmap.ctx, k') key.== (ctx, k)
               then some v
               else #none

  -- | Given an array of keys with index into an irregular array
  -- (old_keys). And an array of tuples with an index which
  -- corresponds to its original position, the shape of the array, and
  -- the constants that will be used for the hash function for the
  -- subarray. This function will return which subarrays for a given
  -- hash function leads to zero collisions. The remaining will get
  -- new constants and a new shape.
  --
  -- precondition: old_keys must not contain duplicate keys.
  def loop_body [n] [w] [m] [q]
                (ctx: ctx)
                (old_rng: rng)
                (idx: int)
                (old_keys: [n](key, int))
                (old_ishape: [w](int, int))
                (old_dest: *[m]int)
                (old_consts: [q]const) : ( rng
                                         , int
                                         , [](key, int)
                                         , [](int, int)
                                         , *[m]int
                                         , []const
                                         ) =
    let (new_rng, consts) = key.rand old_rng
    let (_, old_shape) = unzip old_ishape
    let (flat_size, old_shape_offsets) = old_shape |> exscan (I.+) zero
    let is =
      -- Determine the indices in the flatten array.
      map (\(k, o) ->
             U.(to_uint old_shape_offsets[I.to_i64 o] + (key.hash ctx consts k %% to_uint old_shape[I.to_i64 o])))
          old_keys
    let has_no_collisions =
      -- Collisions in the flatten array.
      rep one
      |> hist (I.+) zero (I.to_i64 flat_size) (map U.to_i64 is)
      |> map (I.<= one)
    let flag_idxs =
      map2 (\i j -> if i I.== zero then neg_one else j) old_shape old_shape_offsets
    let flags =
      -- Flags for the flatten array.
      scatter (rep false) (map I.to_i64 flag_idxs) (rep true)
    let seg_has_no_collision =
      -- A segmented reduce to determine which segments has collisions.
      segmented_reduce (&&) true flags has_no_collisions
      |> sized w
    let new_offsets =
      -- We wanna filter every key that lead to zero collisions. So we
      -- have to update every key that lead to a collision with a new
      -- offset. This is done by mapping every segment which had no
      -- collisions to zero and otherwise to 1.
      map (\f -> if f then zero else one) seg_has_no_collision
      |> scan (I.+) zero
      |> map2 (\f o -> if f then neg_one else o I.- one) seg_has_no_collision
    let new_keys =
      -- Filter every key which lead to no collisions and update the
      -- offset.
      filter (\(_, o) -> not seg_has_no_collision[I.to_i64 o]) old_keys
      |> map (\(key, o) -> (key, new_offsets[I.to_i64 o]))
    let (done, not_done) =
      -- The subarrays are partitioned by if the subarray lead to zero
      -- collisions and is therefore done.
      seg_has_no_collision
      |> zip old_ishape
      |> partition (\(_, f) -> f)
      |> (\(a, b) -> (map (.0) a, map (.0) b))
    let js = map (\(i, _) -> i) done
    let new_dest = scatter old_dest (map I.to_i64 js) (rep idx)
    in ( new_rng
       , idx I.+ one
       , new_keys
       , not_done
       , new_dest
       , old_consts ++ [consts]
       )

  -- | Construct a level two hash function.
  --
  -- precondition: keys must not contain duplicates.
  def from_array_nodup [n] 'v
                       (ctx: ctx)
                       (key_values: [n](key, v)) : ?[f][m].map ctx [n] [f] [m] v =
    let keys = map (.0) key_values
    let values = map (.1) key_values
    let r = key.rng_from_seed [123, i32.i64 n]
    -- The level one hash function is determined here.
    let (r, level_one_const) = key.rand r
    -- The indices to the subarray the keys will land in.
    let is = map U.(to_i64 <-< (%% max (i64 1) (i64 n)) <-< key.hash ctx level_one_const) keys
    -- The number of keys in a given subarray.
    let level_two_counts = hist (I.+) zero n is (rep one)
    -- The shape for each subarray.
    let level_two_shape = map (I.** two) level_two_counts
    let ishape =
      iota n
      |> map I.i64
      |> flip zip level_two_shape
      |> filter (\(_, i) -> i I.!= zero)
    let level_one_offsets =
      -- The initial offsets into the the flatten array for every key.
      map (I.bool <-< (I.!= zero)) level_two_counts
      |> scan (I.+) zero
      |> map2 (\f o -> if f I.!= zero then o I.- one else zero) level_two_counts
    let init_keys = map2 (\k i -> (k, level_one_offsets[i])) keys is
    let dest = replicate n zero
    let (final_r, _, _, _, level_two_consts_indices, level_two_consts) =
      -- Loop until all the hash function have been found that lead
      -- to no collisions.
      loop (old_rng, old_idx, old_keys, old_not_done, old_done, old_consts) =
             (r, zero, init_keys, ishape, dest, [])
      while length old_keys != 0 do
        let ( new_rng
            , new_idx
            , new_keys
            , new_not_done
            , new_done
            , new_consts
            ) =
          loop_body ctx old_rng old_idx old_keys old_not_done old_done old_consts
        in ( new_rng
           , new_idx
           , new_keys
           , new_not_done
           , new_done
           , new_consts
           )
    let (flat_size, level_two_offsets) =
      -- The offsets into the flatten array.
      level_two_shape
      |> exscan (I.+) zero
      |> (\(a, b) -> (a, map2 (\f o -> if f I.== zero then zero else o) level_two_shape b))
    let flat_size' = if n == 0 then 0 else I.to_i64 flat_size
    let level_two =
      transpose [ level_two_offsets
                , level_two_shape
                , level_two_consts_indices
                ]
      :> [n][3]int
    let hash2 =
      I.to_i64
      <-< lookup_flat_index ctx
                            level_one_const
                            level_two_consts
                            level_two
                            flat_size
    let js = map hash2 keys
    let filler =
      if n == 0
      then (#[blank] keys[0], -1)
      else (keys[0], js[0])
    let dest =
      replicate flat_size' (if n == 0
                            then (#[blank] keys[0], #[blank] values[0])
                            else (keys[0], values[0]))
    let (new_keys, new_values) =
      scatter dest js (zip keys values)
      |> unzip
    in { ctx
       , size = rep ()
       , keys = new_keys
       , values = new_values
       , filler = filler
       , level_one_const = level_one_const
       , level_two_consts = level_two_consts
       , level_two = level_two
       , rng = final_r
       }

  def update [n] [f] [m] [u] 'v
             (hmap: map ctx [n] [f] [m] v)
             (key_values: [u](key, v)) =
    let (keys, values) = unzip key_values
    let js = map (I.to_i64 <-< lookup_index hmap.ctx hmap) keys
    -- The smallest indices for each key-value pair.
    let is = hist i64.min i64.highest f js (indices key_values)
    let vs' = hmap.values
    let vs = map2 (\i v -> if i != i64.highest then values[i] else v) is vs'
    in hmap with values = vs

  def from_array_rep_nodup [n] 'v
                           (ctx: ctx)
                           (keys: [n]key)
                           (ne: v) : ?[f][m].(map ctx [n] [f] [m] v) =
    from_array_nodup ctx (zip keys (rep ne))

  def from_array_rep [u] 'v
                     (ctx: ctx)
                     (keys: [u]key)
                     (ne: v) : ?[n][f][m].map ctx [n] [f] [m] v =
    let (_rng, keys) = array.dedup ctx (key.rng_from_seed [i32.i64 u]) keys
    in from_array_rep_nodup ctx keys ne

  def from_array [u] 'v
                 (ctx: ctx)
                 (key_values: [u](key, v)) : ?[n][f][m].map ctx [n] [f] [m] v =
    if u == 0
    then empty ctx
    else let keys' = map (.0) key_values
         let hmap = from_array_rep ctx keys' key_values[0].1
         in update hmap key_values

  def adjust [n] [f] [m] [u] 'v
             (op: v -> v -> v)
             (ne: v)
             (hmap: map ctx [n] [f] [m] v)
             (key_values: [u](key, v)) =
    let (keys, values) = unzip key_values
    let is = map (I.to_i64 <-< lookup_index hmap.ctx hmap) keys
    let vs = reduce_by_index (copy hmap.values) op ne is values
    in hmap with values = vs

  def from_array_hist [u] 'v
                      (ctx: ctx)
                      (op: v -> v -> v)
                      (ne: v)
                      (key_values: [u](key, v)) : ?[n][f][m].map ctx [n] [f] [m] v =
    let keys = map (.0) key_values
    let hmap = from_array_rep ctx keys ne
    in adjust op ne hmap key_values

  def to_array [n] [f] [m] 'v (hmap: map ctx [n] [f] [m] v) : [n](key, v) =
    let keq a b = (hmap.ctx, a) key.== (hmap.ctx, b)
    let (k', i') = hmap.filler
    in zip3 (iota f) hmap.keys hmap.values
       |> filter (\(i, k, _) -> not (keq k k') || i == i')
       |> map (\(_, k, v) -> (k, v))
       |> sized n

  def size [n] [f] [m] 'v (_: map ctx [n] [f] [m] v) =
    n

  def context [n] [f] [m] 'v (hmap: map ctx [n] [f] [m] v) =
    hmap.ctx

  def insert [n] [f] [m] [u] 'v
             (ctx: ctx)
             (hmap: map ctx [n] [f] [m] v)
             (key_values: [u](key, v)) =
    if n == 0 && u == 0
    then hmap
    else let key_values' = to_array hmap
         let (rng, keys) =
           -- First remove keys that are in the hashmap then
           -- deduplicate the keys.
           key_values
           |> map (.0)
           |> filter (\k -> not_member ctx k hmap)
           |> array.dedup ctx (hmap.rng)
         let keys' = map (.0) key_values'
         let filler = if u < n then key_values'[0].1 else key_values[0].1
         let keys'' = keys ++ keys'
         let hmap' = from_array_rep_nodup ctx keys'' filler
         in update hmap' (key_values ++ key_values') with rng = rng

  def insert_with [n] [f] [m] [u] 'v
                  (ctx: ctx)
                  (op: v -> v -> v)
                  (ne: v)
                  (hmap: map ctx [n] [f] [m] v)
                  (key_values: [u](key, v)) =
    let key_values' = to_array hmap
    let (rng, keys) =
      key_values
      |> map (.0)
      |> filter (\k -> not_member ctx k hmap)
      |> array.dedup ctx (hmap.rng)
    let keys' = map (.0) key_values'
    let keys'' = keys ++ keys'
    let hmap' = from_array_rep_nodup ctx keys'' ne
    in adjust op ne hmap' (key_values ++ key_values') with rng = rng

  def reduce [n] [f] [m] 'v (op: v -> v -> v) (ne: v) (hmap: map ctx [n] [f] [m] v) : v =
    let keq a b = (hmap.ctx, a) key.== (hmap.ctx, b)
    let (k', i') = hmap.filler
    in reduce_comm op ne (map3 (\i k v -> if not (keq k k') || i == i' then v else ne)
                            (iota f)
                            hmap.keys
                            hmap.values)

  def map_with_key [n] [f] [m] 'v 't
                   (g: key -> v -> t)
                   (hmap: map ctx [n] [f] [m] v) : map ctx [n] [f] [m] t =
    let keq a b = (hmap.ctx, a) key.== (hmap.ctx, b)
    let (k', i') = hmap.filler
    let vs =
      map3 (\i k v ->
              if not (keq k k') || i == i'
              then g k v
              else #[blank] g k v)
           (iota f)
           hmap.keys
           hmap.values
    in { ctx = hmap.ctx
       , size = hmap.size
       , keys = hmap.keys
       , values = vs
       , filler = hmap.filler
       , level_one_const = hmap.level_one_const
       , level_two_consts = hmap.level_two_consts
       , level_two = hmap.level_two
       , rng = hmap.rng
       }

  def map [n] [f] [m] 'v 't
          (g: v -> t)
          (hmap: map ctx [n] [f] [m] v) : map ctx [n] [f] [m] t =
    map_with_key (\_ v -> g v) hmap
}

-- | Create an implementation of `map`@mtype@"map" using two level hash tables.
-- The key used must have have a universal hash function.
module mk_hashmap_params
  (I: integral)
  (U: integral)
  (K: hashkey with hash = U.t)
  : map
    with key = K.key
    with ctx = K.ctx = {
  module hashmap = mk_two_level_hashmap I U K
  type key = hashmap.key
  type ctx = hashmap.ctx

  type~ map [n] 'v =
    ?[f][m].hashmap.map ctx [n] [f] [m] v

  def from_array [n] 'v (ctx: ctx) (key_values: [n](key, v)) : ?[m].map [m] v =
    hashmap.from_array ctx key_values

  def from_array_rep [n] 'v (ctx: ctx) (keys: [n]key) (ne: v) : ?[m].map [m] v =
    hashmap.from_array_rep ctx keys ne

  def adjust [n] [u] 'v
             (op: v -> v -> v)
             (ne: v)
             (hmap: map [n] v)
             (key_values: [u](key, v)) =
    hashmap.adjust op ne hmap key_values

  def from_array_hist [u] 'v
                      (ctx: ctx)
                      (op: v -> v -> v)
                      (ne: v)
                      (key_values: [u](key, v)) : ?[n].map [n] v =
    hashmap.from_array_hist ctx op ne key_values

  def from_array_nodup [n] 'v (ctx: ctx) (key_values: [n](key, v)) : map [n] v =
    hashmap.from_array_nodup ctx key_values

  def from_array_rep_nodup [n] 'v (ctx: ctx) (keys: [n]key) (ne: v) : map [n] v =
    hashmap.from_array_rep_nodup ctx keys ne

  def reduce [n] 'v (op: v -> v -> v) (ne: v) (hmap: map [n] v) : v =
    hashmap.reduce op ne hmap

  def map [n] 'v 't (g: v -> t) (hmap: map [n] v) : map [n] t =
    hashmap.map g hmap

  def map_with_key [n] 'v 't (g: key -> v -> t) (hmap: map [n] v) : map [n] t =
    hashmap.map_with_key g hmap

  def update [n] [u] 'v (hmap: map [n] v) (key_values: [u](key, v)) =
    hashmap.update hmap key_values

  def to_array [n] 'v (hmap: map [n] v) : [n](key, v) =
    hashmap.to_array hmap

  def size [n] 'v (hmap: map [n] v) =
    hashmap.size hmap

  def member [n] 'v (ctx: ctx) (k: key) (hmap: map [n] v) : bool =
    hashmap.member ctx k hmap

  def not_member [n] 'v (ctx: ctx) (key: key) (hmap: map [n] v) : bool =
    hashmap.not_member ctx key hmap

  def lookup [n] 'v (ctx: ctx) (k: key) (hmap: map [n] v) : opt v =
    hashmap.lookup ctx k hmap

  def context hmap =
    hashmap.context hmap

  def insert [n] [u] 'v
             (ctx: ctx)
             (hmap: map [n] v)
             (key_values: [u](key, v)) =
    hashmap.insert ctx hmap key_values

  def insert_with [n] [u] 'v
                  (ctx: ctx)
                  (op: v -> v -> v)
                  (ne: v)
                  (hmap: map [n] v)
                  (key_values: [u](key, v)) =
    hashmap.insert_with ctx op ne hmap key_values
}

module mk_hashmap = mk_hashmap_params i64 u64

module mk_hashmap_u32 = mk_hashmap_params i32 u32

module type open_addressing_hashmap = {
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

module mk_open_addressing_hashmap
  (K: hashkey with hash = u64)
  (P: {
    val hashmap_size : i64 -> i64
    val probe : u64 -> u64
  })
  : open_addressing_hashmap
    with key = K.key
    with ctx = K.ctx = {
  module key = K
  type const = key.const
  type rng = key.rng
  type key = key.key
  type~ ctx = key.ctx

  local def probe = P.probe
  local def hashmap_size = P.hashmap_size

  type map 'ctx [n] [f] 'v =
    { ctx: ctx
    , const: const
    , keys: [n]key
    , values: [n]v
    , lookup_keys: [f](bool, key)
    , offsets: [f]i64
    , max_iters: u64
    , rng: rng
    }

  local
  def empty 'v ctx : ?[f].map ctx [0] [f] v =
    let r = key.rng_from_seed [123]
    let (r', c) = key.rand r
    in { ctx
       , const = c
       , keys = []
       , values = []
       , offsets = []
       , lookup_keys = []
       , max_iters = 0
       , rng = r'
       }

  def from_array_rep [u] 'v
                     (ctx: ctx)
                     (keys: [u]key)
                     (ne: v) : ?[n][f].map ctx [n] [f] v =
    let r = key.rng_from_seed [123, i32.i64 u]
    let (r, const) = key.rand r
    let filler = (false, keys[0])
    let keq a b = (ctx, a) key.== (ctx, b)
    let hash = key.hash ctx const
    let hashes' = map hash keys
    let size = P.hashmap_size u
    let (max_iters, lookup_keys, _) =
      loop (i, old_dest, old_keys) =
             (0u64, replicate size filler, zip hashes' keys)
      while length old_keys != 0 do
        let hashes =
          map (i64.u64
               <-< (%% u64.i64 size)
               <-< (+ probe i)
               <-< (.0))
              old_keys
        let (is, vs) =
          hist i64.min i64.highest size hashes (indices (old_keys))
          |> map (\i ->
                    if 0 <= i && i < size && !old_dest[hashes[i]].0
                    then (hashes[i], (true, old_keys[i].1))
                    else (-1, filler))
          |> unzip
        let new_dest = scatter (copy old_dest) is vs
        let new_keys =
          zip hashes old_keys
          |> filter (\(i, (_, k)) ->
                       new_dest[i].0
                       && !(k `keq` new_dest[i].1))
          |> map (.1)
        in (i + 1, new_dest, new_keys)
    let keys = filter (.0) lookup_keys |> map (.1)
    let offsets =
      map (i64.bool <-< (.0)) lookup_keys
      |> scan (+) 0
      |> map (+ (-1))
    let s = length keys
    let keys = sized s keys
    in { rng = r
       , const = const
       , keys = keys
       , values = replicate s ne
       , max_iters = max_iters + 1
       , lookup_keys = lookup_keys
       , offsets = offsets
       , ctx = ctx
       }

  def from_array_rep_nodup [n] 'v
                           (ctx: ctx)
                           (keys: [n]key)
                           (ne: v) : ?[f].map ctx [n] [f] v =
    let hmap = from_array_rep ctx keys ne
    in hmap with keys = sized n hmap.keys
            with values = sized n hmap.values

  local
  def lookup_flat_index [n] [f] 'v
                        (ctx: ctx)
                        (k: key)
                        (hmap: map ctx [n] [f] v) : i64 =
    let h = key.hash ctx hmap.const k
    let keq a b = (hmap.ctx, a) key.== (ctx, b)
    in (.2)
       <| loop (is_found, i, j) = (false, 0, -1)
          while i < hmap.max_iters || is_found do
            let idx = i64.u64 ((h + probe i) %% u64.i64 f)
            let (t, k') = hmap.lookup_keys[idx]
            let is_valid = t && (k' `keq` k)
            in ( is_valid
               , i + 1
               , if is_valid then idx else j
               )

  local
  def lookup_index [n] [f] 'v
                   (ctx: ctx)
                   (k: key)
                   (hmap: map ctx [n] [f] v) : i64 =
    let i = lookup_flat_index ctx k hmap
    in if i == -1
       then -1
       else hmap.offsets[i]

  def member [n] [f] 'v
             (ctx: ctx)
             (k: key)
             (hmap: map ctx [n] [f] v) : bool =
    -1 != lookup_flat_index ctx k hmap

  def not_member [n] [f] 'v
                 (ctx: ctx)
                 (k: key)
                 (hmap: map ctx [n] [f] v) : bool =
    not (member ctx k hmap)

  def lookup [n] [f] 'v
             (ctx: ctx)
             (k: key)
             (hmap: map ctx [n] [f] v) : opt v =
    let i = lookup_index ctx k hmap
    in if i == -1
       then #none
       else some hmap.values[i]

  def update [n] [f] [u] 'v
             (hmap: map ctx [n] [f] v)
             (key_values: [u](key, v)) =
    let (keys, values) = unzip key_values
    let js = map (\k -> lookup_index hmap.ctx k hmap) keys
    let is =
      -- The smallest indices for each key-value pair.
      hist i64.min i64.highest n js (indices key_values)
    let vs = map2 (\i v -> if i != i64.highest then values[i] else v) is hmap.values
    in hmap with values = vs

  def from_array [u] 'v
                 (ctx: ctx)
                 (key_values: [u](key, v)) : ?[n][f].map ctx [n] [f] v =
    if u == 0
    then (empty ctx)
    else let (keys, values) = unzip key_values
         let hmap = from_array_rep ctx keys values[0]
         in update hmap key_values

  def from_array_nodup [n] 'v
                       (ctx: ctx)
                       (key_values: [n](key, v)) : ?[f].map ctx [n] [f] v =
    let hmap = from_array ctx key_values
    in hmap with keys = sized n hmap.keys
            with values = sized n hmap.values

  def adjust [n] [f] [u] 'v
             (op: v -> v -> v)
             (ne: v)
             (hmap: map ctx [n] [f] v)
             (key_values: [u](key, v)) =
    let (keys, values) = unzip key_values
    let is = map (\k -> lookup_index hmap.ctx k hmap) keys
    let vs = reduce_by_index (copy hmap.values) op ne is values
    in hmap with values = vs

  def from_array_hist [u] 'v
                      (ctx: ctx)
                      (op: v -> v -> v)
                      (ne: v)
                      (key_values: [u](key, v)) : ?[n][f].map ctx [n] [f] v =
    let keys = map (.0) key_values
    let hmap = from_array_rep ctx keys ne
    in adjust op ne hmap key_values

  def to_array [n] [f] 'v (hmap: map ctx [n] [f] v) : [](key, v) =
    zip hmap.keys hmap.values

  def size [n] [f] 'v (hmap: map ctx [n] [f] v) =
    length hmap.keys

  def context [n] [f] 'v (hmap: map ctx [n] [f] v) =
    hmap.ctx

  def insert [n] [f] [u] 'v
             (ctx: ctx)
             (hmap: map ctx [n] [f] v)
             (key_values: [u](key, v)) =
    if n == 0 && u == 0
    then hmap
    else let key_values' = to_array hmap
         let keys =
           -- First remove keys that are in the hashmap then
           -- deduplicate the keys.
           key_values
           |> map (.0)
           |> filter (\k -> not_member ctx k hmap)
         let keys' = map (.0) key_values'
         let filler = if u < n then key_values'[0].1 else key_values[0].1
         let keys'' = keys ++ keys'
         let hmap' = from_array_rep ctx keys'' filler
         in update hmap' (key_values ++ key_values')

  def insert_with [n] [f] [u] 'v
                  (ctx: ctx)
                  (op: v -> v -> v)
                  (ne: v)
                  (hmap: map ctx [n] [f] v)
                  (key_values: [u](key, v)) =
    let key_values' = to_array hmap
    let keys =
      key_values
      |> map (.0)
      |> filter (\k -> not_member ctx k hmap)
    let keys' = map (.0) key_values'
    let keys'' = keys ++ keys'
    let hmap' = from_array_rep ctx keys'' ne
    in adjust op ne hmap' (key_values ++ key_values')

  def map_with_key [n] [f] 'v 't
                   (g: key -> v -> t)
                   (hmap: map ctx [n] [f] v) : map ctx [n] [f] t =
    let vs = map2 g hmap.keys hmap.values
    in { ctx = hmap.ctx
       , keys = hmap.keys
       , lookup_keys = hmap.lookup_keys
       , values = vs
       , max_iters = hmap.max_iters
       , offsets = hmap.offsets
       , const = hmap.const
       , rng = hmap.rng
       }

  def reduce [n] [f] 'v (op: v -> v -> v) (ne: v) (hmap: map ctx [n] [f] v) : v =
    reduce_comm op ne hmap.values

  def map [n] [f] 'v 't
          (g: v -> t)
          (hmap: map ctx [n] [f] v) : map ctx [n] [f] t =
    map_with_key (\_ v -> g v) hmap
}

module mk_linear_hashmap (K: hashkey with hash = u64)
  : map
    with key = K.key
    with ctx = K.ctx = {
  module hashmap = mk_open_addressing_hashmap K {
    def hashmap_size = (4i64 *)
    def probe = id
  }

  type key = hashmap.key
  type ctx = hashmap.ctx

  type~ map [n] 'v =
    ?[f].hashmap.map ctx [n] [f] v

  def from_array [n] 'v (ctx: ctx) (key_values: [n](key, v)) : ?[m].map [m] v =
    hashmap.from_array ctx key_values

  def from_array_rep [n] 'v (ctx: ctx) (keys: [n]key) (ne: v) : ?[m].map [m] v =
    hashmap.from_array_rep ctx keys ne

  def adjust [n] [u] 'v
             (op: v -> v -> v)
             (ne: v)
             (hmap: map [n] v)
             (key_values: [u](key, v)) =
    hashmap.adjust op ne hmap key_values

  def from_array_hist [u] 'v
                      (ctx: ctx)
                      (op: v -> v -> v)
                      (ne: v)
                      (key_values: [u](key, v)) : ?[n].map [n] v =
    hashmap.from_array_hist ctx op ne key_values

  def from_array_nodup [n] 'v (ctx: ctx) (key_values: [n](key, v)) : map [n] v =
    hashmap.from_array_nodup ctx key_values

  def from_array_rep_nodup [n] 'v (ctx: ctx) (keys: [n]key) (ne: v) : map [n] v =
    hashmap.from_array_rep_nodup ctx keys ne

  def reduce [n] 'v (op: v -> v -> v) (ne: v) (hmap: map [n] v) : v =
    hashmap.reduce op ne hmap

  def map [n] 'v 't (g: v -> t) (hmap: map [n] v) : map [n] t =
    hashmap.map g hmap

  def map_with_key [n] 'v 't (g: key -> v -> t) (hmap: map [n] v) : map [n] t =
    hashmap.map_with_key g hmap

  def update [n] [u] 'v (hmap: map [n] v) (key_values: [u](key, v)) =
    hashmap.update hmap key_values

  def to_array [n] 'v (hmap: map [n] v) : [n](key, v) =
    hashmap.to_array hmap

  def size [n] 'v (hmap: map [n] v) =
    hashmap.size hmap

  def member [n] 'v (ctx: ctx) (k: key) (hmap: map [n] v) : bool =
    hashmap.member ctx k hmap

  def not_member [n] 'v (ctx: ctx) (key: key) (hmap: map [n] v) : bool =
    hashmap.not_member ctx key hmap

  def lookup [n] 'v (ctx: ctx) (k: key) (hmap: map [n] v) : opt v =
    hashmap.lookup ctx k hmap

  def context hmap =
    hashmap.context hmap

  def insert [n] [u] 'v
             (ctx: ctx)
             (hmap: map [n] v)
             (key_values: [u](key, v)) =
    hashmap.insert ctx hmap key_values

  def insert_with [n] [u] 'v
                  (ctx: ctx)
                  (op: v -> v -> v)
                  (ne: v)
                  (hmap: map [n] v)
                  (key_values: [u](key, v)) =
    hashmap.insert_with ctx op ne hmap key_values
}

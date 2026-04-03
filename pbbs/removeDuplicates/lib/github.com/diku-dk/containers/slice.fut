-- | Types representing slices of arrays.
--
-- This module defines various module types and modules for working with slices
-- of arrays at different levels of granularity. None of the functionality here
-- is particularly advanced or difficult to implement, but it is useful as a
-- common interface for other containers.

import "array"
import "key"
import "hash"

-- | An array slice represents a contiguous interval of some array. Essentially,
-- a slice is an offset and a length. The slice does not contain a reference to
-- the array in question. This is mostly useful for working with arrays of
-- slices, as this would otherwise lead to irregular arrays-of-arrays.
module type slice = {
  -- | An array slice. The `a` parameter is merely a phantom type.
  type slice 'a

  -- | Construct an array slice from an offset and a length.
  val mk 'a : (i: i64) -> (n: i64) -> slice a

  -- | Deconstruct array slice into offset and length. The following property
  -- holds:
  --
  -- ```
  -- unmk (mk i n) == (i,n)
  -- ```
  val unmk 'a : slice a -> (i64, i64)

  -- | Retrieve the elements corresponding to some slice of an array. This may
  -- fail if the slide is out-of-bounds.
  val get [n] 'a : slice a -> [n]a -> ?[k].[k]a
}

module slice : slice = {
  type slice 'a = {i: i64, n: i64}

  #[inline]
  def mk i n = {i, n}

  #[inline]
  def unmk {i, n} = (i, n)

  #[inline]
  def get {i, n} xs = xs[i:i + n]
}

-- | Encoder that can encode a type to a sequence of other types.
module type encoder = {
  -- | The type that will be encoded.
  type t

  -- | The type that will be encoded to
  type u

  -- | Get the ith value in the encoding.
  val get : i64 -> t -> u

  -- | The number of values in the final encoding.
  val num : t -> i64
}

module mk_encoder_params
  (U: integral)
  (I: integral)
  : encoder with u = U.t with t = []I.t = {
  type~ t = ?[n].[n]I.t
  type u = U.t

  #[inline]
  def to_u = U.i64 <-< I.to_i64

  #[inline]
  def num_i_per_u : i64 = i64.i32 (U.num_bits / I.num_bits)

  #[inline]
  def num_u_per_i : i64 = i64.i32 (I.num_bits / U.num_bits)

  #[inline]
  def num (ts: t) : i64 =
    if num_i_per_u != 0
    then (length ts + num_i_per_u - 1) / num_i_per_u
    else length ts * num_u_per_i

  #[inline]
  def get (i: i64) (ts: t) : u =
    if num_i_per_u != 0
    then #[unroll]
         loop x = U.i64 0
         for j in 0..<num_i_per_u do
           let idx = i * num_i_per_u + j
           in if idx < length ts
              then let value = U.((to_u ts[idx]) << (i64 j * i32 I.num_bits))
                   in x U.| value
              else x
    else let idx = i / num_u_per_i
         let part = U.(i64 i %% i64 num_u_per_i)
         in if idx < length ts
            then let full_value = to_u ts[idx]
                 let shift_amount = U.(part * i32 num_bits)
                 let mask = U.(U.i64 1 << i32 num_bits - i64 1)
                 in U.(full_value >> shift_amount & mask)
            else U.i64 0
}

module mk_encoder = mk_encoder_params u64
module mk_encoder_u32 = mk_encoder_params u32
module mk_encoder_u16 = mk_encoder_params u16
module mk_encoder_u8 = mk_encoder_params u8

-- | Create a `key`@mtype@"key" for slices of some specific element type. The
-- element type must also be provided with a `key`@mtype module for which the
-- context is unit. You will also need to give an encoder module which can
-- encode the slice to the hash type.
module mk_slice_key
  (K: ordkey with ctx = ())
  (E: encoder with t = []K.key with u = u64)
  : key
    with ctx = []K.key
    with key = slice.slice K.key
    with hash = u64 = {
  type key = slice.slice K.key
  type hash = u64
  type~ ctx = ?[l].[l]K.key

  #[inline]
  def (<=) (xctx: ctx, x: key) (yctx: ctx, y: key) =
    array.le (\x y -> ((), x) K.<= ((), y)) (slice.get x xctx) (slice.get y yctx)

  #[inline]
  def (==) (xctx: ctx, x: key) (yctx: ctx, y: key) =
    array.eq (\x y -> ((), x) K.== ((), y)) (slice.get x xctx) (slice.get y yctx)

  module params = {
    type t = u192
    def n : i64 = 3

    def mat =
      [ [ u192.from_u64 ([0u64, 28754346u64, 11015437201764459397u64] :> [u192.n]u64)
        , u192.from_u64 ([0u64, 33446050u64, 44765319426087909u64] :> [u192.n]u64)
        , u192.from_u64 ([0u64, 5193344u64, 11774702713623250524u64] :> [u192.n]u64)
        , u192.from_u64 ([0u64, 2000789u64, 2890036977250326397u64] :> [u192.n]u64)
        ]
      , [ u192.from_u64 ([0u64, 7039301u64, 3152774312296373500u64] :> [u192.n]u64)
        , u192.from_u64 ([0u64, 6411676u64, 3833190130467790283u64] :> [u192.n]u64)
        , u192.from_u64 ([0u64, 27067162u64, 9418817281394045152u64] :> [u192.n]u64)
        , u192.from_u64 ([0u64, 22737421u64, 8745441487662595665u64] :> [u192.n]u64)
        ]
      , [ u192.from_u64 ([0u64, 23154442u64, 6850429895295465782u64] :> [u192.n]u64)
        , u192.from_u64 ([0u64, 24350097u64, 16396793026104248044u64] :> [u192.n]u64)
        , u192.from_u64 ([0u64, 9164704u64, 5882893599925425177u64] :> [u192.n]u64)
        , u192.from_u64 ([0u64, 11265513u64, 3093699834540157620u64] :> [u192.n]u64)
        ]
      ]
      :> [n][n + 1]u192
  }

  module engine = mk_ndimlcg u192 params

  type rng = engine.rng
  type const = [params.n]u192

  def rng_from_seed = engine.rng_from_seed

  def rand r =
    u192.(let (r, t) = engine.rand r
          let t0 = if t[0] == zero then one else t[0]
          let t2 = if t[2] == zero then one else t[2]
          in (r, sized params.n [t0, t[1], t2]))

  #[inline]
  def hash (ctx: []K.key) (a: const) (x: key) : hash =
    let data = slice.get x ctx
    let n = E.num data
    in universal.hash_string a[0] a[1] a[2] E.get n data
}

module mk_slice_key_u32
  (K: ordkey with ctx = ())
  (E: encoder with t = []K.key with u = u32)
  : key
    with ctx = []K.key
    with key = slice.slice K.key
    with hash = u32 = {
  type key = slice.slice K.key
  type~ ctx = ?[l].[l]K.key
  type uint = u32
  type hash = u32

  #[inline]
  def (<=) (xctx: ctx, x: key) (yctx: ctx, y: key) =
    array.le (\x y -> ((), x) K.<= ((), y)) (slice.get x xctx) (slice.get y yctx)

  #[inline]
  def (==) (xctx: ctx, x: key) (yctx: ctx, y: key) =
    array.eq (\x y -> ((), x) K.== ((), y)) (slice.get x xctx) (slice.get y yctx)

  module params = {
    type t = u128
    def n : i64 = 3

    def mat =
      [ [ u128.from_u64 ([0u64, 130376333404382465u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 941437598877963704u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 983855883704868386u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 474909089031450842u64] :> [u128.n]u64)
        ]
      , [ u128.from_u64 ([0u64, 2291080586977276207u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 1774766946851307907u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 862284600868831119u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 1919656809696445781u64] :> [u128.n]u64)
        ]
      , [ u128.from_u64 ([0u64, 1560860746668876372u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 2065326635364188065u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 923420781333808644u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 1659270389580084573u64] :> [u128.n]u64)
        ]
      ]
      :> [n][n + 1]u128
  }

  module engine = mk_ndimlcg u128 params

  type rng = engine.rng
  type const = [params.n]u128

  def rng_from_seed = engine.rng_from_seed

  def rand r =
    u128.(let (r, t) = engine.rand r
          let t0 = if t[0] == zero then one else t[0]
          let t2 = if t[2] == zero then one else t[2]
          in (r, sized params.n [t0, t[1], t2]))

  #[inline]
  def hash (ctx: []K.key) (a: const) (x: key) : hash =
    let data = slice.get x ctx
    let n = E.num data
    in universal_u32.hash_string a[0] a[1] a[2] E.get n data
}

-- | Create a `key`@mtype@"key" for slices using FNV hashing.
module mk_static_slice_key
  (K: ordkey with ctx = ())
  (E: encoder with t = []K.key with u = u8)
  : key
    with ctx = []K.key
    with key = slice.slice K.key
    with hash = u64
    with const = ()
    with rng = () = {
  type key = slice.slice K.key
  type hash = u64
  type~ ctx = ?[l].[l]K.key
  type const = ()
  type rng = ()

  def rng_from_seed [n] (_seed: [n]i32) : rng = ()

  def rand (r: rng) : (rng, const) = (r, ())

  #[inline]
  def (<=) (xctx: ctx, x: key) (yctx: ctx, y: key) =
    array.le (\x y -> ((), x) K.<= ((), y)) (slice.get x xctx) (slice.get y yctx)

  #[inline]
  def (==) (xctx: ctx, x: key) (yctx: ctx, y: key) =
    array.eq (\x y -> ((), x) K.== ((), y)) (slice.get x xctx) (slice.get y yctx)

  #[inline]
  def hash (ctx: []K.key) (_: const) (x: key) : hash =
    let data = slice.get x ctx
    in fnv.hash E.get (E.num data) data
}

module mk_static_slice_key_u32
  (K: ordkey with ctx = ())
  (E: encoder with t = []K.key with u = u8)
  : key
    with ctx = []K.key
    with key = slice.slice K.key
    with hash = u32
    with const = ()
    with rng = () = {
  type key = slice.slice K.key
  type hash = u32
  type~ ctx = ?[l].[l]K.key
  type const = ()
  type rng = ()

  def rng_from_seed [n] (_seed: [n]i32) : rng = ()

  def rand (r: rng) : (rng, const) = (r, ())

  #[inline]
  def (<=) (xctx: ctx, x: key) (yctx: ctx, y: key) =
    array.le (\x y -> ((), x) K.<= ((), y)) (slice.get x xctx) (slice.get y yctx)

  #[inline]
  def (==) (xctx: ctx, x: key) (yctx: ctx, y: key) =
    array.eq (\x y -> ((), x) K.== ((), y)) (slice.get x xctx) (slice.get y yctx)

  #[inline]
  def hash (ctx: []K.key) (_: const) (x: key) : hash =
    let data = slice.get x ctx
    in fnv_u32.hash E.get (E.num data) data
}

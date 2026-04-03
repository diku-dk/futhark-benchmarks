-- | Definitions of modules that implement both the `hashkey`@mtype@"hashkey"
-- and `ordkey`@mtype@"ordkey" module types for a variety of types.

import "../cpprandom/random"
open import "hashkey"
open import "ordkey"
import "hash"

module type key = {
  type ctx
  type key
  type const
  type hash

  include hashkey
  with ctx = ctx
  with key = key
  with const = const
  with hash = hash

  include ordkey
  with ctx = ctx
  with key = key
}

local
module mk_int_key
  (P: {
    type t

    val to_i64 : t -> i64
    val (==) : t -> t -> bool
    val (<=) : t -> t -> bool
  })
  : key with ctx = () with key = P.t with hash = u64 = {
  type key = P.t
  type ctx = ()
  type hash = u64

  module params = {
    type t = u192
    def n : i64 = 2

    def mat =
      [ [ u192.from_u64 ([0u64, 4550289u64, 3114443612905851817u64] :> [u192.n]u64)
        , u192.from_u64 ([0u64, 31521294u64, 4677272142619895914u64] :> [u192.n]u64)
        , u192.from_u64 ([0u64, 4309077u64, 17725230307779815399u64] :> [u192.n]u64)
        ]
      , [ u192.from_u64 ([0u64, 32864941u64, 14067805869423985411u64] :> [u192.n]u64)
        , u192.from_u64 ([0u64, 7570401u64, 4379138438894234111u64] :> [u192.n]u64)
        , u192.from_u64 ([0u64, 31318884u64, 8032588423207853219u64] :> [u192.n]u64)
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
          in (r, sized params.n [t0, t[1]]))

  #[inline]
  def hash _ (cs: const) (x: key) : hash =
    let x' = u64.i64 (P.to_i64 x)
    in universal.hash cs[0] cs[1] x'

  #[inline]
  def (==) (_, x) (_, y) = x P.== y

  #[inline]
  def (<=) (_, x) (_, y) = x P.<= y
}

module u8key : key with ctx = () with key = u8 with hash = u64 = mk_int_key u8
module u16key : key with ctx = () with key = u16 with hash = u64 = mk_int_key u16
module u32key : key with ctx = () with key = u32 with hash = u64 = mk_int_key u32
module u64key : key with ctx = () with key = u64 with hash = u64 = mk_int_key u64

module i8key : key with ctx = () with key = i8 with hash = u64 = mk_int_key i8
module i16key : key with ctx = () with key = i16 with hash = u64 = mk_int_key i16
module i32key : key with ctx = () with key = i32 with hash = u64 = mk_int_key i32
module i64key : key with ctx = () with key = i64 with hash = u64 = mk_int_key i64

local
module mk_int_key_u32
  (P: {
    type t

    val to_i64 : t -> i64
    val (==) : t -> t -> bool
    val (<=) : t -> t -> bool
  })
  : key with ctx = () with key = P.t with hash = u32 = {
  type key = P.t
  type ctx = ()
  type hash = u32

  module params = {
    type t = u128
    def n : i64 = 2

    def mat =
      [ [ u128.from_u64 ([0u64, 2293423045107085747u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 800780595221070060u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 2091102638183227686u64] :> [u128.n]u64)
        ]
      , [ u128.from_u64 ([0u64, 594268281314473874u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 122033425430844521u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 1566285142180431642u64] :> [u128.n]u64)
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
          in (r, sized params.n [t0, t[1]]))

  #[inline]
  def hash _ (cs: const) (x: key) : hash =
    let x' = u32.i64 (P.to_i64 x)
    in universal_u32.hash cs[0] cs[1] x'

  #[inline]
  def (==) (_, x) (_, y) = x P.== y

  #[inline]
  def (<=) (_, x) (_, y) = x P.<= y
}

local
module mk_int_key_u32_64bit
  (P: {
    type t

    val to_i64 : t -> i64
    val (==) : t -> t -> bool
    val (<=) : t -> t -> bool
  })
  : key with ctx = () with key = P.t with hash = u32 = {
  type key = P.t
  type ctx = ()
  type hash = u32

  module params = {
    type t = u128
    def n : i64 = 4

    def mat =
      [ [ u128.from_u64 ([0u64, 1889274433039334491u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 1558719663069797306u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 898800792483617031u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 182671347101262303u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 524728102339812979u64] :> [u128.n]u64)
        ]
      , [ u128.from_u64 ([0u64, 2138874755982552324u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 1027391227346557362u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 1385211329054194292u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 1624796925062206517u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 2225966430211402310u64] :> [u128.n]u64)
        ]
      , [ u128.from_u64 ([0u64, 420038697015483829u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 1470917168597411235u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 1730635935960627482u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 1913445897167741427u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 1958922128221574794u64] :> [u128.n]u64)
        ]
      , [ u128.from_u64 ([0u64, 49989825992718695u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 822540154684993876u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 2169992429208474226u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 269517618746531594u64] :> [u128.n]u64)
        , u128.from_u64 ([0u64, 2206842482050090356u64] :> [u128.n]u64)
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
          in (r, sized params.n [t0, t[1], t2, t[3]]))

  #[inline]
  def hash _ (cs: const) (x: key) : hash =
    let x' = u64.i64 (P.to_i64 x)
    let lo = u32.u64 x'
    let hi = u32.u64 (x' >> 32)
    in universal_u32.hash_vector [(cs[0], cs[1]), (cs[2], cs[3])]
                                 [lo, hi]

  #[inline]
  def (==) (_, x) (_, y) = x P.== y

  #[inline]
  def (<=) (_, x) (_, y) = x P.<= y
}

module u8key_u32 : key with ctx = () with key = u8 with hash = u32 = mk_int_key_u32 u8
module u16key_u32 : key with ctx = () with key = u16 with hash = u32 = mk_int_key_u32 u16
module u32key_u32 : key with ctx = () with key = u32 with hash = u32 = mk_int_key_u32 u32
module u64key_u32 : key with ctx = () with key = u64 with hash = u32 = mk_int_key_u32_64bit u64

module i8key_u32 : key with ctx = () with key = i8 with hash = u32 = mk_int_key_u32 i8
module i16key_u32 : key with ctx = () with key = i16 with hash = u32 = mk_int_key_u32 i16
module i32key_u32 : key with ctx = () with key = i32 with hash = u32 = mk_int_key_u32 i32
module i64key_u32 : key with ctx = () with key = i64 with hash = u32 = mk_int_key_u32_64bit i64

local
module mk_static_int_key
  (P: {
    type t

    val to_i64 : t -> i64
    val (==) : t -> t -> bool
    val (<=) : t -> t -> bool
    val num_bits : i32
  })
  : key with ctx = () with key = P.t with hash = u64 with const = () with rng = () = {
  type key = P.t
  type ctx = ()
  type hash = u64
  type const = ()
  type rng = ()

  def rng_from_seed [n] (_seed: [n]i32) : rng = ()

  def rand (r: rng) : (rng, const) = (r, ())

  local
  #[inline]
  def num_bytes = i64.i32 (P.num_bits / 8)

  #[inline]
  def hash _ (_: const) (x: key) : hash =
    let x_u64 = u64.i64 (P.to_i64 x)
    let get_byte (i: i64) (_: u64): u8 = u8.u64 ((x_u64 >> (u64.i64 i * 8)) & 0xFF)
    in fnv.hash get_byte num_bytes x_u64

  #[inline]
  def (==) (_, x) (_, y) = x P.== y

  #[inline]
  def (<=) (_, x) (_, y) = x P.<= y
}

module static_u8key : key with ctx = () with key = u8 with hash = u64 with const = () with rng = () = mk_static_int_key u8
module static_u16key : key with ctx = () with key = u16 with hash = u64 with const = () with rng = () = mk_static_int_key u16
module static_u32key : key with ctx = () with key = u32 with hash = u64 with const = () with rng = () = mk_static_int_key u32
module static_u64key : key with ctx = () with key = u64 with hash = u64 with const = () with rng = () = mk_static_int_key u64

module static_i8key : key with ctx = () with key = i8 with hash = u64 with const = () with rng = () = mk_static_int_key i8
module static_i16key : key with ctx = () with key = i16 with hash = u64 with const = () with rng = () = mk_static_int_key i16
module static_i32key : key with ctx = () with key = i32 with hash = u64 with const = () with rng = () = mk_static_int_key i32
module static_i64key : key with ctx = () with key = i64 with hash = u64 with const = () with rng = () = mk_static_int_key i64

local
module mk_static_int_key_u32
  (P: {
    type t

    val to_i64 : t -> i64
    val (==) : t -> t -> bool
    val (<=) : t -> t -> bool
    val num_bits : i32
  })
  : key with ctx = () with key = P.t with hash = u32 with const = () with rng = () = {
  type key = P.t
  type ctx = ()
  type hash = u32
  type const = ()
  type rng = ()

  def rng_from_seed [n] (_seed: [n]i32) : rng = ()

  def rand (r: rng) : (rng, const) = (r, ())

  local
  #[inline]
  def num_bytes = i64.i32 (P.num_bits / 8)

  #[inline]
  def hash _ (_: const) (x: key) : hash =
    let x_u64 = u64.i64 (P.to_i64 x)
    let get_byte (i: i64) (_: u64): u8 = u8.u64 ((x_u64 >> (u64.i64 i * 8)) & 0xFF)
    in fnv_u32.hash get_byte num_bytes x_u64

  #[inline]
  def (==) (_, x) (_, y) = x P.== y

  #[inline]
  def (<=) (_, x) (_, y) = x P.<= y
}

module static_u8key_u32 : key with ctx = () with key = u8 with hash = u32 with const = () with rng = () = mk_static_int_key_u32 u8
module static_u16key_u32 : key with ctx = () with key = u16 with hash = u32 with const = () with rng = () = mk_static_int_key_u32 u16
module static_u32key_u32 : key with ctx = () with key = u32 with hash = u32 with const = () with rng = () = mk_static_int_key_u32 u32
module static_u64key_u32 : key with ctx = () with key = u64 with hash = u32 with const = () with rng = () = mk_static_int_key_u32 u64

module static_i8key_u32 : key with ctx = () with key = i8 with hash = u32 with const = () with rng = () = mk_static_int_key_u32 i8
module static_i16key_u32 : key with ctx = () with key = i16 with hash = u32 with const = () with rng = () = mk_static_int_key_u32 i16
module static_i32key_u32 : key with ctx = () with key = i32 with hash = u32 with const = () with rng = () = mk_static_int_key_u32 i32
module static_i64key_u32 : key with ctx = () with key = i64 with hash = u32 with const = () with rng = () = mk_static_int_key_u32 i64

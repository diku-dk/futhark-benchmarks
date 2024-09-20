-- | A non-comparison-based sort that sorts an array in *O(k n)* work
-- and *O(k log(n))* span, where *k* is the number of bits in each element.
-- The library contains two variants of radix sort with different use
-- cases. `blocked_radix_sort`@term should be used on large arrays,
-- if the array is small then it may be the case that
-- `radix_sort`@term is faster. `radix_sort`@term can also be much
-- more suitable in cases where nested parallelism is utilized.
--
-- Generally, this is the sorting function we recommend for Futhark
-- programs, but be careful about negative integers (use
-- `radix_sort_int`@term) and floating-point numbers (use
-- `radix_sort_float`@term).  If you need a comparison-based sort,
-- consider `merge_sort`@term@"merge_sort".
--
-- ## See Also
--
-- * `merge_sort`@term@"merge_sort"

local def radix_sort_step [n] 't (xs: [n]t) (get_bit: i32 -> t -> i32)
                                 (digit_n: i32): [n]t =
  let num x = get_bit (digit_n+1) x * 2 + get_bit digit_n x
  let pairwise op (a1,b1,c1,d1) (a2,b2,c2,d2) =
    (a1 `op` a2, b1 `op` b2, c1 `op` c2, d1 `op` d2)
  let bins = xs |> map num
  let flags = bins |> map (\x ->
      ( i64.bool (x==0)
      , i64.bool (x==1)
      , i64.bool (x==2)
      , i64.bool (x==3) ) )
  let offsets = scan (pairwise (+)) (0,0,0,0) flags
  let (na,nb,nc,_nd) = last offsets
  let f bin (a,b,c,d) = (-1)
      + a * (i64.bool (bin == 0)) + na * (i64.bool (bin > 0))
      + b * (i64.bool (bin == 1)) + nb * (i64.bool (bin > 1))
      + c * (i64.bool (bin == 2)) + nc * (i64.bool (bin > 2))
      + d * (i64.bool (bin == 3)) 
  let is = map2 f bins offsets
  in scatter (copy xs) is xs

-- | The `num_bits` and `get_bit` arguments can be taken from one of
-- the numeric modules of module type `integral`@mtype@"/prelude/math"
-- or `float`@mtype@"/prelude/math", such as `i32`@term@"/prelude/math"
-- or `f64`@term@"/prelude/math".  However, if you know that
-- the input array only uses lower-order bits (say, if all integers
-- are less than 100), then you can profitably pass a smaller
-- `num_bits` value to reduce the number of sequential iterations.
--
-- **Warning:** while radix sort can be used with numbers, the bitwise
-- representation of of both integers and floats means that negative
-- numbers are sorted as *greater* than non-negative.  Negative floats
-- are further sorted according to their absolute value.  For example,
-- radix-sorting `[-2.0, -1.0, 0.0, 1.0, 2.0]` will produce `[0.0,
-- 1.0, 2.0, -1.0, -2.0]`.  Use `radix_sort_int`@term and
-- `radix_sort_float`@term in the (likely) cases that this is not what
-- you want.
def radix_sort [n] 't (num_bits: i32) (get_bit: i32 -> t -> i32)
                      (xs: [n]t): [n]t =
  let iters = if n == 0 then 0 else (num_bits+2-1)/2
  in loop xs for i < iters do radix_sort_step xs get_bit (i*2)

def with_indices [n] 'a (xs: [n]a) : [n](a, i64) =
  zip xs (iota n)

local def by_key_wrapper [n] 't sorter key num_bits get_bit (xs: [n]t) : [n]t =
  map key xs
  |> with_indices
  |> sorter num_bits (\i (k, _) -> get_bit i k)
  |> map (\(_, i: i64) -> xs[i]) -- OK because '0<=i<n'.

-- | Like `radix_sort`, but sort based on key function.
def radix_sort_by_key [n] 't 'k
    (key: t -> k)
    (num_bits: i32) (get_bit: i32 -> k -> i32) (xs: [n]t): [n]t =
  by_key_wrapper radix_sort key num_bits get_bit xs

-- | A thin wrapper around `radix_sort`@term that ensures negative
-- integers are sorted as expected.  Simply pass the usual `num_bits`
-- and `get_bit` definitions from e.g. `i32`@term@"/prelude/math".
def radix_sort_int [n] 't (num_bits: i32) (get_bit: i32 -> t -> i32)
                          (xs: [n]t): [n]t =
  let get_bit' i x =
    -- Flip the most significant bit.
    let b = get_bit i x
    in if i == num_bits-1 then b ^ 1 else b
  in radix_sort num_bits get_bit' xs

-- | Like `radix_sort_int`, but sort based on key function.
def radix_sort_int_by_key [n] 't 'k
    (key: t -> k)
    (num_bits: i32) (get_bit: i32 -> k -> i32) (xs: [n]t): [n]t =
  by_key_wrapper radix_sort_int key num_bits get_bit xs

-- | A thin wrapper around `radix_sort`@term that ensures floats are
-- sorted as expected.  Simply pass the usual `num_bits` and `get_bit`
-- definitions from `f32`@term@"/prelude/math" and
-- `f64`@term@"/prelude/math".
def radix_sort_float [n] 't (num_bits: i32) (get_bit: i32 -> t -> i32)
                            (xs: [n]t): [n]t =
  let get_bit' i x =
    -- We flip the bit returned if:
    --
    -- 0) the most significant bit is set (this makes more negative
    --    numbers sort before less negative numbers), or
    --
    -- 1) we are asked for the most significant bit (this makes
    --    negative numbers sort before positive numbers).
    let b = get_bit i x
    in if get_bit (num_bits-1) x == 1 || i == num_bits-1
       then b ^ 1 else b
  in radix_sort num_bits get_bit' xs

-- | Like `radix_sort_float`, but sort based on key function.
def radix_sort_float_by_key [n] 't 'k
    (key: t -> k)
    (num_bits: i32) (get_bit: i32 -> k -> i32) (xs: [n]t): [n]t =
  by_key_wrapper radix_sort_float key num_bits get_bit xs

local def exscan op ne xs =
  let s =
    scan op ne xs
    |> rotate (-1)
  let s[0] = ne
  in s

local def get_bin 't
                  (k: i32)
                  (get_bit: i32 -> t -> i32)
                  (digit_n: i32)
                  (x: t): i64  =
  i64.i32 <|
  loop acc = 0 for i < k do
    acc + (get_bit (digit_n + i) x << i)
  
local def radix_sort_step_i16 [n] 't
                              (get_bit: i32 -> t -> i32)
                              (digit_n: i32)
                              (xs: [n]t):
                              ([n]t, [4]i64, [4]i16) =
  let num x = i16.i32 (get_bit (digit_n+1) x * 2 + get_bit digit_n x)
  let pairwise op (a1,b1,c1,d1) (a2,b2,c2,d2) =
    (a1 `op` a2, b1 `op` b2, c1 `op` c2, d1 `op` d2)
  let bins = xs |> map num
  let flags = bins |> map (\x ->
      ( i16.bool (x==0)
      , i16.bool (x==1)
      , i16.bool (x==2)
      , i16.bool (x==3) ) )
  let offsets = scan (pairwise (+)) (0,0,0,0) flags
  let (na,nb,nc,nd) = if n == 0 then (0,0,0,0) else last offsets
  let f bin (a,b,c,d) = i64.i16 ((-1)
      + a * (i16.bool (bin == 0)) + na * (i16.bool (bin > 0))
      + b * (i16.bool (bin == 1)) + nb * (i16.bool (bin > 1))
      + c * (i16.bool (bin == 2)) + nc * (i16.bool (bin > 2))
      + d * (i16.bool (bin == 3))) 
  let is = map2 f bins offsets
  in (scatter (copy xs) is xs
     ,map i64.i16 [na, nb, nc, nd]
     ,[0, na, na + nb, na + nb + nc])

local def blocked_radix_sort_step [n] [m] [r] 't
                          (get_bit: i32 -> t -> i32)
                          (digit_n: i32)
                          (xs: *[n*m+r]t) =
  let (blocks, rest) = split xs
  let (sorted_rest, hist_rest, offsets_rest) =
    radix_sort_step_i16 get_bit digit_n rest
  let (sorted_blocks, hist_blocks, offsets_blocks) =
    unflatten blocks
    |> map (radix_sort_step_i16 get_bit digit_n)
    |> unzip3
  let histograms = hist_blocks ++ [hist_rest]
  let sorted = sized (n*m+r) (flatten sorted_blocks ++ sorted_rest)
  let old_offsets = offsets_blocks ++ [offsets_rest]
  let new_offsets =
    histograms
    |> transpose
    |> flatten
    |> exscan (+) 0
    |> unflatten
    |> transpose
  let is =
    tabulate (n * m + r) (
      \i ->
        let elem = sorted[i]
        let bin = get_bin 2 get_bit digit_n elem
        let block_idx = i / m
        let new_offset = new_offsets[block_idx][bin]
        let old_block_offset = i64.i16 old_offsets[block_idx][bin]
        let old_offset = m * block_idx + old_block_offset
        let idx = (i - old_offset) + new_offset
        in idx
    )
  in scatter xs is sorted

local def (///) (a: i32) (b: i32) : i32 =
  a / b + i32.bool (a % b != 0)

local def (////) (a: i64) (b: i64) : i64 =
  a / b + i64.bool (a % b != 0)

-- | This implementation of radix sort is based on an algorithm which
-- splits the input up into blocks, sorts them and collect them [1].
-- This leads to performance gains if you choose a good `block` size
-- based on the GPU thread block. Using 256 as the `block` size to
-- sort 100 million i32 with an A100 GPU leads to a 2x speedup
-- compared to `radix_sort`@term. The sorting algorithm is stable and
-- its work is *O(k n)* and the span is *O(k log(n))* where *k* is the
-- number of bits in the elements being sorted. In the analysis of the
-- asymptotics we assume the `block` size is some constant.
--
-- [1] N. Satish, M. Harris and M. Garland, "Designing efficient
-- sorting algorithms for manycore GPUs," 2009 IEEE International
-- Symposium on Parallel & Distributed Processing, Rome, Italy, 2009,
-- pp. 1-10, doi: 10.1109/IPDPS.2009.5161005.
def blocked_radix_sort [n] 't
                       (block: i16)
                       (num_bits: i32)
                       (get_bit: i32 -> t -> i32)
                       (xs: [n]t): [n]t =
  let iters = if n == 0 then 0 else (num_bits + 2 - 1) / 2
  let block = i64.i16 block
  let n_blocks = n / block
  let rest = n % block
  let xs = sized (n_blocks * block + rest) xs
  in sized n <|
     loop xs = copy xs for i < iters do
       blocked_radix_sort_step get_bit (i * 2) xs

-- | Like `radix_sort_by_key` but blocked.
def blocked_radix_sort_by_key [n] 't 'k
                              (block: i16)
                              (key: t -> k)
                              (num_bits: i32)
                              (get_bit: i32 -> k -> i32)
                              (xs: [n]t): [n]t =
  let sorter = blocked_radix_sort block
  in by_key_wrapper sorter key num_bits get_bit xs

-- | Like `radix_sort_by_int` but blocked.
def blocked_radix_sort_int [n] 't
                           (block: i16)
                           (num_bits: i32)
                           (get_bit: i32 -> t -> i32)
                           (xs: [n]t): [n]t =
  let get_bit' i x =
    let b = get_bit i x
    in if i == num_bits-1 then b ^ 1 else b
  in blocked_radix_sort block num_bits get_bit' xs

-- | Like `radix_sort_int_by_key` but blocked.
def blocked_radix_sort_int_by_key [n] 't 'k
                                  (block: i16)
                                  (key: t -> k)
                                  (num_bits: i32)
                                  (get_bit: i32 -> k -> i32)
                                  (xs: [n]t): [n]t =
  let sorter = blocked_radix_sort_int block
  in by_key_wrapper sorter key num_bits get_bit xs

-- | Like `radix_sort_float` but blocked.
def blocked_radix_sort_float [n] 't
                             (block: i16)
                             (num_bits: i32)
                             (get_bit: i32 -> t -> i32)
                             (xs: [n]t): [n]t =
  let get_bit' i x =
    let b = get_bit i x
    in if get_bit (num_bits-1) x == 1 || i == num_bits-1
       then b ^ 1 else b
  in blocked_radix_sort block num_bits get_bit' xs

-- | Like `radix_sort_float_by_key` but blocked.
def blocked_radix_sort_float_by_key [n] 't 'k
                                    (block: i16)
                                    (key: t -> k)
                                    (num_bits: i32)
                                    (get_bit: i32 -> k -> i32)
                                    (xs: [n]t): [n]t =
  let sorter = blocked_radix_sort_float block
  in by_key_wrapper sorter key num_bits get_bit xs

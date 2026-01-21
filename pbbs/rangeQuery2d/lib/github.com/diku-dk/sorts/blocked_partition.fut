-- | Blocked partition.
--
-- This module contains

local
def partition_block [n] 't
                    (p: t -> bool)
                    (xs: [n]t) : ([n]t, [2]i64, [2]i16) =
  let pairwise op (a1, b1) (a2, b2) = (a1 `op` a2, b1 `op` b2)
  let flags =
    xs
    |> map (\x ->
              ( i16.bool <| not <| p x
              , i16.bool <| p x
              ))
  let offsets = scan (pairwise (+)) (0, 0) flags
  let (na, nb) = if n == 0 then (0, 0) else last offsets
  let f bin (a, b) =
    i64.i16 ((-1)
             + a * (i16.bool (bin == 0))
             + na * (i16.bool (bin > 0))
             + b * (i16.bool (bin == 1)))
  let bins = xs |> map (i16.bool <-< p)
  let is = map2 f bins offsets
  in ( scatter (copy xs) is xs
     , map i64.i16 [na, nb]
     , [0, na]
     )

local
def exscan op ne xs =
  let s =
    scan op ne xs
    |> rotate (-1)
  let s[0] = ne
  in s

local
def blocked_partition_auxiliary [n] [m] [r] 't
                                (p: t -> bool)
                                (xs: [n * m + r]t) : ([n * m + r]t, i64) =
  let (blocks, rest) = split xs
  let (partitioned_rest, hist_rest, offsets_rest) =
    partition_block p rest
  let (partitioned_blocks, hist_blocks, offsets_blocks) =
    unflatten blocks
    |> map (partition_block p)
    |> unzip3
  let histograms = hist_blocks ++ [hist_rest]
  let partitions = sized (n * m + r) (flatten partitioned_blocks ++ partitioned_rest)
  let old_offsets = offsets_blocks ++ [offsets_rest]
  let new_offsets =
    histograms
    |> transpose
    |> flatten
    |> exscan (+) 0
    |> unflatten
    |> transpose
  let is =
    tabulate (n * m + r) (\i ->
                            let elem = partitions[i]
                            let bin = i64.bool <| p elem
                            let block_idx = i / m
                            let new_offset = new_offsets[block_idx][bin]
                            let old_block_offset = i64.i16 old_offsets[block_idx][bin]
                            let old_offset = m * block_idx + old_block_offset
                            let idx = (i - old_offset) + new_offset
                            in idx)
  let out = replicate (n * m + r) xs[0] :> [n * m + r]t
  in (scatter out is partitions, new_offsets[0][1])

-- | A blocked partition, this leads to a speed up compared to the
-- partition given in Futharks prelude. It works much like a blocked
-- radix sort [1] but just with one bit. It probably leads to worse
-- fusion and is only worth using for large enough inputs.
--
-- [1] N. Satish, M. Harris and M. Garland, "Designing efficient
-- sorting algorithms for manycore GPUs," 2009 IEEE International
-- Symposium on Parallel & Distributed Processing, Rome, Italy, 2009,
-- pp. 1-10, doi: 10.1109/IPDPS.2009.5161005.
def blocked_partition [n] 't
                      (block: i16)
                      (p: t -> bool)
                      (xs: [n]t) : ?[k].([k]t, [n - k]t) =
  let block = i64.i16 block
  let n_blocks = n / block
  let rest = n % block
  let xs = sized (n_blocks * block + rest) xs
  let (ys, o) = blocked_partition_auxiliary (not <-< p) xs
  in (ys[0:o], ys[o:n])

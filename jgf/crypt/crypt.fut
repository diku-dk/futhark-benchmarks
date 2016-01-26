-- Implementation of Bruce Schneiers IDEA block cipher.  Based on
-- IDEATest.jomp from the JGF benchmark suite.  Comments too.
--
-- ==
-- input @ crypt-data/small.in
-- output @ crypt-data/small.out

-- We represent 16-bit blocks as 32-bit integers so they can be more
-- easily treated as unsigned.
fun i32 mk16b(i8 upper, i8 lower) =
  (i32(upper) & 0xFFi32) << 8 | (i32(lower) & 0xFFi32)

fun [i8,8] cipher_idea_block([i16,52] key, [i8,8] block) =
  -- Translate key to i32 for convenience.
  let key = map(&0xFFFF, map(i32, key)) in
  let x1 = mk16b(block[1], block[0]) in
  let x2 = mk16b(block[3], block[2]) in
  let x3 = mk16b(block[5], block[4]) in
  let x4 = mk16b(block[7], block[6]) in
  loop ({x1,x2,x3,x4}) = for i < 8 do
    let ik = i * 6 in
    -- 1) Multiply (modulo 0x10001), 1st text sub-block with 1st key
    -- sub-block.
    let x1 = i32(i64(x1) * i64(key[ik+0]) %% i64(0x10001i64) & 0xFFFFi64) in
    -- 2) Add (modulo 0x10000), 2nd text sub-block with 2nd key
    -- sub-block.
    let x2 = x2 + key[ik+1] & 0xFFFF in
    -- 3) Add (modulo 0x10000), 3rd text sub-block with 3rd key
    -- sub-block.
    let x3 = x3 + key[ik+2] & 0xFFFF in
    -- 4) Multiply (modulo 0x10001), 4th text sub-block
    -- with 4th key sub-block.
    let x4 = i32(i64(x4) * i64(key[ik+3]) %% i64(0x10001i64) & 0xFFFFi64) in
    -- 5) XOR results from steps 1 and 3.
    let t2 = x1 ^ x3 in
    -- 6) XOR results from steps 2 and 4.  Included in step 8.

    -- 7) Multiply (modulo 0x10001), result of step 5 with 5th key
    -- sub-block.
    let t2 = i32(i64(t2) * i64(key[ik+4]) %% 0x10001i64 & 0xFFFFi64) in
    -- 8) Add (modulo 0x10000), results of steps 6 and 7.
    let t1 = t2 + (x2 ^ x4) & 0xFFFFi32 in
    -- 9) Multiply (modulo 0x10001), result of step 8 with 6th key
    -- sub-block.
    let t1 = i32(i64(t1) * i64(key[ik+5]) % 0x10001i64 & 0xFFFFi64) in
    -- 10) Add (modulo 0x10000), results of steps 7 and 9.
    let t2 = t1 + t2 & 0xFFFF in
    -- 11) XOR results from steps 1 and 9.
    let x1 = x1 ^ t1 in
    -- 14) XOR results from steps 4 and 10. (Out of order).
    let x4 = x4 ^ t2 in
    -- 13) XOR results from steps 2 and 10. (Out of order).
    let t2 = t2 ^ x2 in
    -- 12) XOR results from steps 3 and 9. (Out of order).
    let x2 = x3 ^ t1 in
    let x3 = t2 in -- Results of x2 and x3 now swapped.
    {x1,x2,x3,x4} in
  let ik = 6 * 8 in
  -- Final output transform (4 steps).

  -- 1) Multiply (modulo 0x10001), 1st text-block
  -- with 1st key sub-block.
  let x1 = i32(i64(x1) * i64(key[ik+0]) %% 0x10001i64 & 0xFFFFi64) in
  -- 2) Add (modulo 0x10000), 2nd text sub-block
  -- with 2nd key sub-block. It says x3, but that is to undo swap
  -- of subblocks 2 and 3 in 8th processing round.
  let x3 = x3 + i32(key[ik+1]) & 0xFFFF in
  -- 3) Add (modulo 0x10000), 3rd text sub-block
  -- with 3rd key sub-block. It says x2, but that is to undo swap
  -- of subblocks 2 and 3 in 8th processing round.
  let x2 = x2 + i32(key[ik+2]) & 0xFFFF in
  -- 4) Multiply (modulo 0x10001), 4th text-block with 4th key
  -- sub-block.
  let x4 = i32(i64(x4) * i64(key[ik+3]) %% 0x10001i64 & 0xFFFFi64) in
  -- Repackage from 16-bit sub-blocks to 8-bit byte array text2.
  [ i8(x1), i8(x1>>>8)
  , i8(x3), i8(x3>>>8)
  , i8(x2), i8(x2>>>8)
  , i8(x4), i8(x4>>>8)
  ]

fun [i8,n] cipher_idea([i16,52] key, [i8,n] text) =
  let blocks = reshape((n//8,8), text) in
  reshape((n), map(cipher_idea_block(key), blocks))

fun {[i8,n], [i8,n]} main([i16,52] Z, [i16,52] DK, [i8,n] text) =
  let text_encrypted = cipher_idea(Z, text) in
  {text_encrypted, cipher_idea(DK, text_encrypted)}

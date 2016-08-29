-- Implementation of Bruce Schneiers IDEA block cipher.  Based on
-- IDEATest.jomp from the JGF benchmark suite.  Comments too.
--
-- ==
-- nobench input @ crypt-data/small.in
-- output @ crypt-data/small.out
-- notravis input @ crypt-data/medium.in
-- output @ crypt-data/medium.out

fun mk16b(upper: u8, lower: u8): u16 =
  (u16(upper) & 0xFFu16) << 8u16 | (u16(lower) & 0xFFu16)

fun cipher_idea_block(key: [52]u16) (block: [8]u8): [8]u8 =
  let x1 = mk16b(block[1], block[0])
  let x2 = mk16b(block[3], block[2])
  let x3 = mk16b(block[5], block[4])
  let x4 = mk16b(block[7], block[6])
  loop ((x1,x2,x3,x4)) = for i < 8 do
    let ik = i * 6
    -- 1) Multiply (modulo 0x10001), 1st text sub-block with 1st key
    -- sub-block.
    let x1 = u16(u32(x1) * u32(key[ik+0]) %% 0x10001u32)
    -- 2) Add (modulo 0x10000), 2nd text sub-block with 2nd key
    -- sub-block.
    let x2 = x2 + key[ik+1]
    -- 3) Add (modulo 0x10000), 3rd text sub-block with 3rd key
    -- sub-block.
    let x3 = x3 + key[ik+2]
    -- 4) Multiply (modulo 0x10001), 4th text sub-block
    -- with 4th key sub-block.
    let x4 = u16(u32(x4) * u32(key[ik+3]) %% 0x10001u32)
    -- 5) XOR results from steps 1 and 3.
    let t2 = x1 ^ x3
    -- 6) XOR results from steps 2 and 4.  Included in step 8.

    -- 7) Multiply (modulo 0x10001), result of step 5 with 5th key
    -- sub-block.
    let t2 = u16(u32(t2) * u32(key[ik+4]) %% 0x10001u32)
    -- 8) Add (modulo 0x10000), results of steps 6 and 7.
    let t1 = t2 + (x2 ^ x4)
    -- 9) Multiply (modulo 0x10001), result of step 8 with 6th key
    -- sub-block.
    let t1 = u16(u32(t1) * u32(key[ik+5]) %% 0x10001u32)
    -- 10) Add (modulo 0x10000), results of steps 7 and 9.
    let t2 = t1 + t2
    -- 11) XOR results from steps 1 and 9.
    let x1 = x1 ^ t1
    -- 14) XOR results from steps 4 and 10. (Out of order).
    let x4 = x4 ^ t2
    -- 13) XOR results from steps 2 and 10. (Out of order).
    let t2 = t2 ^ x2
    -- 12) XOR results from steps 3 and 9. (Out of order).
    let x2 = x3 ^ t1
    let x3 = t2 in -- Results of x2 and x3 now swapped.
    (x1,x2,x3,x4)
  let ik = 6 * 8
  -- Final output transform (4 steps).

  -- 1) Multiply (modulo 0x10001), 1st text-block
  -- with 1st key sub-block.
  let x1 = u16(u32(x1) * u32(key[ik+0]) %% 0x10001u32)
  -- 2) Add (modulo 0x10000), 2nd text sub-block
  -- with 2nd key sub-block. It says x3, but that is to undo swap
  -- of subblocks 2 and 3 in 8th processing round.
  let x3 = x3 + key[ik+1]
  -- 3) Add (modulo 0x10000), 3rd text sub-block
  -- with 3rd key sub-block. It says x2, but that is to undo swap
  -- of subblocks 2 and 3 in 8th processing round.
  let x2 = x2 + key[ik+2]
  -- 4) Multiply (modulo 0x10001), 4th text-block with 4th key
  -- sub-block.
  let x4 = u16(u32(x4) * u32(key[ik+3]) %% 0x10001u32)
  -- Repackage from 16-bit sub-blocks to 8-bit byte array text2.
  in [ u8(x1), u8(x1>>>8u16)
     , u8(x3), u8(x3>>>8u16)
     , u8(x2), u8(x2>>>8u16)
     , u8(x4), u8(x4>>>8u16)
     ]

fun cipher_idea(key: [52]u16, text: [n]u8): [n]u8 =
  let blocks = reshape (n//8,8) text
  in reshape (n) (map (cipher_idea_block(key)) blocks)

fun main(z: [52]u16, dk: [52]u16, text: [n]u8): ([n]u8, [n]u8) =
  let text_encrypted = cipher_idea(z, text)
  in (text_encrypted, cipher_idea(dk, text_encrypted))

-- Not part of the benchmark as such, but maybe it is interesting by
-- itself.  This program computes the encryption- and decryption-keys
-- from a user-key.  Also adopted from the Java implementation.
-- Sequential, so don't expect anything cool here.
--
-- ==
-- input @ crypt-data/userkey0.txt
-- output @ crypt-data/keys0.txt

-- Multiplicative inverse mod 0x10001.
let inv(a: i16): i32 =
  let a = i32(a)&0xFFFF
  let b = 0x10001
  let u = 0
  let v = 1
  let (_,_,u,_) = loop ((a,b,u,v)) while a > 0i32 do
    let q = i32((i64(b)&0xFFFFFFFFi64) // (i64(a)&0xFFFFi64))
    let r = i32((i64(b)&0xFFFFFFFFi64) %% (i64(a)&0xFFFFi64))

    let b = i32(a)
    let a = r

    let t = v
    let v = u - q * v
    let u = t
    in (a,b,u,v)

  in (if u < 0 then u + 0x10001 else u) & 0xFFFF

let encryptionKey(userkey: [8]i16): [52]i16 =
  -- Key starts out blank.
  let z = replicate 52 0i16
  -- First 8 subkeys are userkey itself.
  let z = loop (z) for i < 8 do
    let z[i] = userkey[i]
    in z
  -- Each set of 8 subkeys thereafter is derived from left rotating
  -- the whole 128-bit key 25 bits to left (once between each set of
  -- eight keys and then before the last four). Instead of actually
  -- rotating the whole key, this routine just grabs the 16 bits
  -- that are 25 bits to the right of the corresponding subkey
  -- eight positions below the current subkey. That 16-bit extent
  -- straddles two array members, so bits are shifted left in one
  -- member and right (with zero fill) in the other. For the last
  -- two subkeys in any group of eight, those 16 bits start to
  -- wrap around to the first two members of the previous eight.
  let z = loop (z) for i in map (8+) (iota (52-8)) do
    let j = i %% 8
    let z[i] = if      j  < 6 then (z[i-7]>>>9i16) | (z[i-6]<<7i16)
               else if j == 6 then (z[i-7]>>>9i16) | (z[i-14]<<7i16)
                              else (z[i-15]>>>9i16) | (z[i-14]<<7i16)
    in z
  in z

let decryptionKey(z: [52]i16): [52]i16 =
  -- Key starts out blank.
  let dk = replicate 52 0i16
  let t1 = inv(z[0])
  let t2 = i32(-z[1]) & 0xFFFF
  let t3 = i32(-z[2]) & 0xFFFF
  let dk[51] = i16(inv(z[3]))
  let dk[50] = i16(t3)
  let dk[49] = i16(t2)
  let dk[48] = i16(t1)
  let dk = loop (dk) for i < 7 do
    let kb = 4 + 6 * i
    let jb = 47 - 6 * i
    let t1 = z[kb+0]
    let dk[jb-0] = z[kb+1]
    let dk[jb-1] = t1
    let t1 = i16(inv(z[kb+2]))
    let t2 = -z[kb+3]
    let t3 = -z[kb+4]
    let dk[jb-2] = i16(inv(z[kb+5]))
    let dk[jb-3] = t2
    let dk[jb-4] = t3
    let dk[jb-5] = t1
    in dk
  let kb = 4 + 6 * 7
  let jb = 47 - 6 * 7

  let t1 = z[kb+0]
  let dk[jb-0] = z[kb+1]
  let dk[jb-1] = t1
  let t1 = i16(inv(z[kb+2]))
  let t2 = -z[kb+3]
  let t3 = -z[kb+4]
  let dk[jb-2] = i16(inv(z[kb+5]))
  let dk[jb-3] = t3
  let dk[jb-4] = t2
  let dk[jb-5] = t1
  in dk

let main(userkey: [8]i16): ([52]i16,[52]i16) =
  let z = encryptionKey(userkey)
  in (z, decryptionKey(z))

-- Simple seven-point stencil - in three dimensions, which is at least
-- a little interesting.
--
-- Parboil has both 'large' and 'default' data sets, but they are
-- identical, so we only have 'large'.  In fact, we don't even have
-- 'large' - I just generated some random garbage of the same size.
-- The running time is not data-sensitive here anyway.
--
-- Actually, we don't even have garbage of the same size (too big for
-- Github), so we replicate a smaller slice.  And slice it for the
-- output.  Should not affect running time much.
--
-- ==
-- compiled input @ data/large.in
-- output @ data/large.out

let next (c0: f32) (c1: f32) (A0: [#nx][#ny][#nz]f32) (i: i32) (j: i32) (k: i32): f32 =
  -- We map the edge to itself.
  if i == 0 || i == nx-1 || j == 0 || j == ny-1 || k == 0 || k == nz-1
  then A0[i,j,k]
  else unsafe (A0[i,j,k+1] + A0[i,j,k-1] +
               A0[i,j-1,k] + A0[i,j+1,k] +
               A0[i-1,j,k] + A0[i+1,j,k]) * c1
              + A0[i,j,k] * c0


let main (iterations: i32) (nx: i32) (A0_slice: [#ny][#nz]f32): [ny][nz]f32 =
  -- c0 and c1 are also hardcoded in Rodinia.
  let c0 = 1.0f32/6.0f32
  let c1=1.0f32/6.0f32/6.0f32

  loop (A0 = replicate nx A0_slice) = for _i < iterations do
    map (\i -> map (\j -> map (\k -> next c0 c1 A0 i j k) (iota nz)) (iota ny)) (iota nx)

  in A0[0]

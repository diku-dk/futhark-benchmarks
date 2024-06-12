-- Port based on the CPU implementation in Parboil.
--
-- Anyway, performance is okayish.  On the small data set, Parboil
-- runs 4 seconds on the CPU, and Futhark runs in 7.5 seconds.  On the
-- GPU, on the large dataset, we run in 39ms, and Parboil (CUDA) runs
-- in something like 4ms (but this does not include all memsets and
-- allocations, whilst the Futhark runtime does include some).
--
-- The biggest kernel is essentially a map-with-reduce where we just
-- sequentialise the reduction and run a map.  Parboil does essentially the
-- same thing, but they also play some tricks with constant memory.  I
-- tried replicating this in Futhark, but it did not affect performance at
-- all.  They also have an unrolled loop.  Again, I tried replicating it,
-- but it only brought us down to 32ms.
--
-- After this I got really upset and tried out Parboil's OpenCL
-- implementation, whose kernel runs in 44ms using what appears to be the
-- same algorithm.  I suspect NVIDIA/CUDA trickery!
--
-- I even tried commenting out the memory reads entirely, and it barely
-- affected the runtime.  _However_, changing the calls to sin() and cos()
-- to merely divide by 2 reduces the kernel runtime to a third.
--
-- Adding -cl-unsafe-math-optimizations to the kernel compiler command line
-- also brings down kernel runtime from 32ms to 12ms, even with the calls
-- to sin() and cos().
--
-- I am not sure what to check next.
-- ==
-- compiled input @ data/small.in
-- output @ data/small.out
-- compiled input @ data/large.in
-- output @ data/large.out

def main [numK][numX]
         (kx: [numK]f32) (ky: [numK]f32) (kz: [numK]f32)
         (x: [numX]f32) (y: [numX]f32) (z: [numX]f32)
         (phiR: [numK]f32) (phiI: [numK]f32)
       : ([numX]f32, [numX]f32) =
  let phiMag = phiR*phiR + phiI*phiI
  let expArgs = 2*f32.pi*(kx*transpose (replicate numK x)
                + ky*transpose (replicate numK y)
                + kz*transpose (replicate numK z))
  let qr = f32.sum (f32.cos expArgs * phiMag)
  let qi = f32.sum (f32.sin expArgs * phiMag)
  in (qr, qi)

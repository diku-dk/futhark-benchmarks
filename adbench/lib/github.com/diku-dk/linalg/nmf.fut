-- | Non-negative matrix factorisation.
--
-- Much of this code is based on work by Kasper Unn Weihe, Kristian
-- Quirin Hansen, and Peter Kanstrup Larsen.  See [their
-- report](https://futhark-lang.org/student-projects/kristian-kasper-peter-project.pdf)
-- for details.

import "../cpprandom/random"
import "linalg"

-- | NMF with *Multiplicative Update Rules* and using the Frobenius
-- norm to determine divergence.
module mk_nmf (T: real) : {
  val nmf [m][n] : (A: [m][n]T.t)
                -> (k: i64)
                -> (max_iter: i64)
                -> (tol: T.t)
                -> ([m][k]T.t, [k][n]T.t, i64)
  val frob_norm [m][n] : (xss: [m][n]T.t) -> T.t
} = {
module rng_engine = minstd_rand
module rand_T = normal_distribution T u32 rng_engine
module norm_dist = normal_distribution T u32 minstd_rand

def sum = reduce (T.+) (T.i64 0)

def dotprod [n] (xs: [n]T.t) (ys: [n]T.t): T.t =
  sum (map2 (T.*) xs ys)

def matmul [n][p][m] (xss: [n][p]T.t) (yss: [p][m]T.t): *[n][m]T.t =
  map (\xs -> map (dotprod xs) (transpose yss)) xss

def matmul_scalar [m][n] (xss: [m][n]T.t) (k: T.t): *[m][n]T.t =
  map (map (\x -> x T.* k)) xss

def matdiv_entrywise [m][n] (xss: [m][n]T.t) (yss: [m][n]T.t): *[m][n]T.t =
  map2 (map2 (T./)) xss yss

def matmul_entrywise [m][n] (xss: [m][n]T.t) (yss: [m][n]T.t): *[m][n]T.t =
  map2 (map2 (T.*)) xss yss

def matsub [m][n] (xss: [m][n]T.t) (yss: [m][n]T.t): *[m][n]T.t =
  map2 (\xs ys -> map2 (T.-) xs ys) xss yss

def matmean [m][n] (xss: [m][n]T.t): T.t =
  (sum (map (\x -> reduce (T.+) (T.i64 0) x) xss)) T./ T.i64(m*n)

def sqrt x = T.(x ** (i64 1 / i64 2))

def frob_norm [m][n] (xss: [m][n]T.t): T.t =
  let abs_matrix = map(map (\x -> x T.* x)) xss
  let matrix_sum = sum (map sum abs_matrix)
  in sqrt matrix_sum

def stream (n: i64) (low: T.t) (high: T.t) =
  let rng_state = rng_engine.rng_from_seed [123]
  let rng_states = rng_engine.split_rng n rng_state
  let (_,rng) = unzip (map (norm_dist.rand {mean = low, stddev = high}) rng_states)
  in rng

def stream2d (m: i64) (n: i64) (low: T.t) (high: T.t) =
  unflatten (stream (m * n) low high)

def random_init [m][n] (A: [m][n]T.t) (k: i64): (*[m][k]T.t, *[k][n]T.t) =
  let avg = sqrt T.(matmean A / i64(k))
  let W = matmul_scalar (stream2d m k (T.i64 0) (T.i64 1)) avg
  let H = matmul_scalar (stream2d k n (T.i64 0) (T.i64 1)) avg
  let W_abs = map(map T.abs) W
  let H_abs = map(map T.abs) H
  in (W_abs, H_abs)

def nmf [m][n] (A: [m][n]T.t) (k: i64) (max_iter: i64) (tol: T.t): ([m][k]T.t, [k][n]T.t, i64) =
  let (W, H) = random_init A k
  let init_norm = frob_norm(matsub A (matmul W H))
  let (W, H, n_iter, _, _, _) =
    loop
      (W, H, n_iter, previous_norm, current_norm, diverged) =
      (W,H, 0, init_norm, T.i64 0, false)
    while ((n_iter < max_iter) && !diverged) do

    -- Update H
    let W_TA = matmul (transpose W) A
    let W_TWH = matmul (transpose W) (matmul W H)
    let H_update = matdiv_entrywise W_TA W_TWH
    let H = matmul_entrywise H H_update

    -- Update W
    let AH_T = matmul A (transpose H)
    let WHH_T = matmul W (matmul H (transpose H))
    let W_update = matdiv_entrywise AH_T WHH_T
    let W = matmul_entrywise W W_update

    let check_per = 10

    in if tol T.> T.i64 0 && n_iter % check_per == 0 then
       let current_norm = frob_norm(matsub A (matmul W H))
       in if T.((previous_norm - current_norm) / init_norm < tol)
          then let diverged = true
               in (W, H, n_iter, previous_norm, current_norm, diverged)
          else let previous_norm = current_norm
               let n_iter = n_iter+1
               in (W, H, n_iter, previous_norm, current_norm, diverged)
       else let n_iter = n_iter+1
            in (W, H, n_iter, previous_norm, current_norm, diverged)
  in (W, H, n_iter)
}

-- Translation to Futhark: https://www.cs.cmu.edu/~scandal/alg/connectivity.html

def parent_edges [n] [m] (parents: [n]i64) (edges: [m](i64, i64)) =
  map (\(v, u) -> (parents[v], parents[u])) edges

def shrink_edges [n] [m] (parents: [n]i64) (edges: [m](i64, i64)) =
  parent_edges parents edges
  |> filter (uncurry (!=))

def flip_edges [n] (edges: [n](i64, i64)) (flags: [n]bool) =
  map2 (\f (v, u) -> if f then (u, v) else (v, u)) flags edges

def direct_edges [n] (edges: [n](i64, i64)) =
  edges ++ flip_edges edges (rep true)

def shortcut [n] (parents: [n]i64) =
  map (\v -> parents[v]) parents

def sv_init [n] (ps: [n]i64) (qs: *[n]i64) (iter: i64) : *([n]i64, [n]i64) =
  let gps = shortcut ps
  let updates =
    filter (\(p, gp) -> gp != p) (zip ps gps)
    |> map (.1)
  let qs' = scatter qs updates (rep iter)
  in (gps, qs')

def sv_cond_hook [n] [m]
                 (newps: *[n]i64)
                 (ps: [n]i64)
                 (qs: *[n]i64)
                 (es: [m](i64, i64))
                 (iter: i64) : *([n]i64, [n]i64) =
  let newp_es1 = parent_edges newps es
  let pfroms = map (\(v, _) -> ps[v]) es
  let newp_es2 =
    filter (\((newpfrom, newpto), pfrom) ->
              newpfrom == pfrom && newpto < newpfrom)
           (zip newp_es1 pfroms)
    |> map (.0)
  let (hook_froms, hook_tos) = unzip newp_es2
  let newps' = scatter newps hook_froms hook_tos
  let qs' = scatter qs hook_tos (rep iter)
  in (newps', qs')

def sv_stagnant_p (p: i64) (gp: i64) (qp: i64) (iter: i64) : bool =
  p == gp && qp < iter

def sv_uncond_hook [n] [m]
                   (ps: *[n]i64)
                   (qs: [n]i64)
                   (es: [m](i64, i64))
                   (iter: i64) : *[n]i64 =
  let pes = parent_edges ps es
  let pfroms = map (.0) pes
  let gpfroms = map (\pfrom -> ps[pfrom]) pfroms
  let qpfroms = map (\pfrom -> qs[pfrom]) pfroms
  let updates =
    filter (\((pfrom, pto), gpfrom, qpfrom) ->
              sv_stagnant_p pfrom gpfrom qpfrom iter && pfrom != pto)
           (zip3 pes gpfroms qpfroms)
    |> map (.0)
  let (hook_froms, hook_tos) = unzip updates
  in scatter ps hook_froms hook_tos

def any_eq [n] (target: i64) (arr: [n]i64) : bool =
  reduce (||) false (map (== target) arr)

def sv_alg1 [n] [m]
            (ps: [n]i64)
            (qs: [n]i64)
            (es: [m](i64, i64))
            (max_iters: i64) : [n]i64 =
  let (ps_final, _, _, _) =
    loop (ps, qs, iter, continue) = copy (ps, qs, 0i64, true)
    while continue && iter < max_iters do
      let (ps1, qs1) = sv_init ps qs iter
      let (ps2, qs2) = sv_cond_hook ps1 ps qs1 es iter
      let ps3 = sv_uncond_hook ps2 qs2 es iter
      let any_updated = any_eq iter qs2
      in if !any_updated
         then (ps3, qs2, iter + 1, false)
         else (shortcut ps3, qs2, iter + 1, true)
  in ps_final

def cc_sv1 [m] (edges: [m](i64, i64)) (n: i64) : [n]i64 =
  let ps = iota n
  let qs = replicate n 0
  let es = direct_edges edges
  let max_iters = 64 - i64.i32 (i64.clz n)
  in sv_alg1 ps qs es max_iters

-- | Construction of Euler Tours.
--
-- For a tree with `n` numbered nodes, the Euler tour E is represented by an
-- array of pairs, such that `E[i] = (l,r)` gives the position of the opening
-- and closing parenthesis in the DFS parenthetisation of the tree.

import "../../sorts/radix_sort"

local
def wyllie_scan_step [n] 'a
                     (op: a -> a -> a)
                     (values: [n]a)
                     (P: [n]i64) =
  let f i =
    if P[i] == -1
    then (values[i], P[i])
    else (values[i] `op` values[P[i]], P[P[i]])
  in unzip (tabulate n f)

local
def wyllie_scan [n] 'a
                (op: a -> a -> a)
                (values: [n]a)
                (P: [n]i64) =
  let (values, _) =
    loop (values, P) for _i < 64 - i64.clz n do
      wyllie_scan_step op values P
  in values

local
def preorder_pos [n] (P: [n]i64) (children: [n - 1]i64) (starts: [n]i64) (root: i64) =
  let P_of_children: []i64 = map (\v -> P[v]) children
  let sedges: []{p: i64, u: i64} =
    radix_sort_int_by_key (.p)
                          64i32
                          (\bit k -> i32.i64 ((k >> i64.i32 bit) & 1i64))
                          (map2 (\p u -> {p, u}) P_of_children children)
  let sp = map (.p) sedges
  let su = map (.u) sedges
  let flags: []i64 =
    tabulate (n - 1) \i ->
      if i == 0
      then 1
      else i64.bool (sp[i] != sp[i - 1])
  let marks: []i64 =
    map2 (\i f -> if f == 1 then i else -1) (indices sp) flags
  let group_start: []i64 =
    scan i64.max (-1) marks
  let rank_in_group: []i64 =
    map2 (i64.-) (indices sp) group_start
  let pos_in_P_sorted: []i64 =
    map2 (\p r -> starts[p] + i64.bool (p != root) + r) sp rank_in_group
  let pos_in_P: [n]i64 =
    spread n (-1) su pos_in_P_sorted
  in pos_in_P

-- | Given a parent vector representation of a tree, where the root is indicated
-- by being its own parent (but may be located anywhere), compute the Euler tour
-- of the tree.
def euler_tour 'a [n] (P: [n]i64) =
  if n == 1
  then replicate n (0, 1)
  else let root: i64 =
         i64.minimum (map2 (\v p -> if p == v then v else i64.highest) (iota n) P)
       let children: []i64 = sized (n - 1) (filter (\v -> v != root) (iota n))
       let child_count: [n]i64 =
         hist (i64.+) 0i64 n (map2 (\i p -> if i == root then -1 else p) (iota n) P) (rep 1)
       let segd: [n]i64 =
         tabulate n \v -> child_count[v] + i64.bool (v != root)
       let starts: [n]i64 = exscan (i64.+) 0i64 segd
       let m: i64 = i64.sum segd
       let P_slot: [n]i64 =
         tabulate n \v -> if v == root then -1i64 else starts[v]
       let pos_in_pre: [n]i64 =
         preorder_pos P children starts root
       --
       let child_slots: []i64 =
         map (\u -> P_slot[u]) children
       let P_slots: []i64 =
         map (\u -> pos_in_pre[u]) children
       let cross1: [m]i64 = spread m (-1) child_slots P_slots
       let cross: [m]i64 = scatter cross1 P_slots child_slots
       let owner: [m]i64 = scan i64.max (-1i64) (spread m (-1) starts (iota n))
       let next_in_seg: [m]i64 =
         tabulate m \i ->
           let v = owner[i]
           let s = starts[v]
           let deg = segd[v]
           in if i + 1i64 < s + deg then i + 1i64 else s
       let succ: [m]i64 =
         map (\c -> next_in_seg[c]) cross
       let pred: [m]i64 =
         spread m (-1) succ (iota m) with [starts[root]] = -1i64
       let init_vals: [m]i64 =
         replicate m 1i64 with [starts[root]] = 0i64
       let rank: [m]i64 =
         wyllie_scan (i64.+) init_vals pred
       let lp: [n]i64 =
         tabulate n \v ->
           if v == root
           then 0
           else 1 + rank[pos_in_pre[v]]
       let rp: [n]i64 =
         tabulate n \v ->
           if v == root
           then 2 * n - 1
           else 1 + rank[P_slot[v]]
       in zip lp rp

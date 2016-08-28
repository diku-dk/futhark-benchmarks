-- Variant of game of life with a donut world.
--
-- Rules invented by Torben Mogensen.
--
-- This is a variant where only orthogonal neighbours are considered.
--
-- ==
-- tags { disable }

fun sum_of_cell_and_neighbors(i: int, j: int, board: [n][m]i8): i8 =
  unsafe
  let above = (i - 1) % n in
  let below = (i + 1) % n in
  let right = (j + 1) % m in
  let left = (j - 1) % m in
  board[above,j] +
  board[i,left] + board[i,j] + board[i,right] +
  board[below,j]

fun all_neighbour_sums(board: [n][m]i8): [n][m]i8 =
  map (fn (i: int): [m]i8  =>
        map (fn (j: int): i8  =>
              sum_of_cell_and_neighbors(i,j,board)
           ) (iota(m))
     ) (iota(n))

fun iteration(board: [n][m]i8): [n][m]i8 =
  let all_sums = all_neighbour_sums(board) in
  map (fn (row_sums: []i8): [m]i8  =>
        map (fn (s: i8): i8  =>
              let t = [0i8, 1i8, 1i8, 0i8,
                       0i8, 1i8, 1i8, 1i8,
                       2i8, 2i8, 2i8, 3i8,
                       3i8, 2i8, 2i8, 3i8]
              in unsafe t[int(s)]
           ) (row_sums)
     ) (all_sums)

fun min(x: int, y: int): int =
  if x < y then x else y

entry init(world: [n][m]bool): ([n][m]i8, [n][m]int) =
  (map (fn (row: []bool): [m]i8  =>
         map (fn (b: bool): i8  =>
               if b then 1i8 else 0i8) row) world,
   replicate(n, replicate(m, 0 << 2)))

entry render_frame(all_history: [n][m]int): [n][m][3]i8 =
  map (fn (row_history: []int): [m][3]i8  =>
        map (colour_history) (row_history)
     ) (all_history)

fun colour_history(history: int): [3]i8 =
  let used_to_be = history & 3 -- Last two bits encode the previous live cell.
  let age = min(255, history >> 2)
  let colours = [[0i8,   0i8,   255i8],
                 [0i8,   255i8, 0i8],
                 [255i8, 0i8,   0i8],
                 [255i8, 255i8, 0i8]]
  let colour = unsafe colours[used_to_be]
  in map (-i8(age)) colour

fun update_history(history: int, now: i8): int =
  let used_to_be = history & 3 -- Last two bits encode the previous live cell.
  let age = min(128, history >> 2)
  in if now == i8(used_to_be)
     then (((age + 1) << 2) | int(now))
     else int(now)

entry steps(world: [n][m]i8, history: [n][m]int, steps: int): ([n][m]i8, [n][m]int) =
  loop ((world, history)) = for i < steps do
    (let world' = iteration(world)
     let history' = zipWith (fn (row_history: []int, row: []i8): [m]int  =>
                              zipWith (update_history) (row_history) row) history (world')
     in (world', history'))
  in (world, history)

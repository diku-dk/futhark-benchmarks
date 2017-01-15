-- Variant of game of life with a donut world.
--
-- Rules invented by Torben Mogensen.
--
-- This is a variant where only orthogonal neighbours are considered.
--
-- ==
-- tags { disable }

fun sum_of_cell_and_neighbors(i: i32, j: i32, board: [n][m]i8): i8 =
  unsafe
  let above = (i - 1) % n
  let below = (i + 1) % n
  let right = (j + 1) % m
  let left = (j - 1) % m
  in board[above,j] +
     board[i,left] + board[i,j] + board[i,right] +
     board[below,j]

fun all_neighbour_sums(board: [n][m]i8): [n][m]i8 =
  map (\(i: i32): [m]i8  ->
        map (\(j: i32): i8  ->
              sum_of_cell_and_neighbors(i,j,board)
           ) (iota(m))
     ) (iota(n))

fun iteration(board: [n][m]i8): [n][m]i8 =
  let all_sums = all_neighbour_sums(board) in
  map (\(row_sums: []i8): [m]i8  ->
        map (\(s: i8): i8  ->
              let t = [0i8, 1i8, 1i8, 0i8,
                       0i8, 1i8, 1i8, 1i8,
                       2i8, 2i8, 2i8, 3i8,
                       3i8, 2i8, 2i8, 3i8]
              in unsafe t[i32(s)]
           ) (row_sums)
     ) (all_sums)

fun min(x: i32, y: i32): i32 =
  if x < y then x else y

entry init(world: [n][m]bool): ([n][m]i8, [n][m]i32) =
  (map (\(row: []bool): [m]i8  ->
         map (\(b: bool): i8  ->
               if b then 1i8 else 0i8) row) world,
   replicate n (replicate m (0 << 2)))

entry render_frame(all_history: [n][m]i32): [n][m][3]i8 =
  map (\(row_history: []i32): [m][3]i8  ->
         map colour_history row_history)
      all_history

fun colour_history(history: i32): [3]i8 =
  let used_to_be = history & 3 -- Last two bits encode the previous live cell.
  let age = min(255, history >> 2)
  let colours = [[0i8,   0i8,   255i8],
                 [0i8,   255i8, 0i8],
                 [255i8, 0i8,   0i8],
                 [255i8, 255i8, 0i8]]
  let colour = unsafe colours[used_to_be]
  in map (-i8(age)) colour

fun update_history(history: i32) (now: i8): i32 =
  let used_to_be = history & 3 -- Last two bits encode the previous live cell.
  let age = min(128, history >> 2)
  in if now == i8(used_to_be)
     then (((age + 1) << 2) | i32(now))
     else i32(now)

entry steps(world: [n][m]i8, history: [n][m]i32, steps: i32): ([n][m]i8, [n][m]i32) =
  loop ((world, history)) = for i < steps do
    (let world' = iteration(world)
     let history' = map (\(row_history: []i32) (row: []i8): [m]i32  ->
                              map update_history row_history row) history world'
     in (world', history'))
  in (world, history)

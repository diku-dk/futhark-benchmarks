-- Variant of game of life with a donut world.
--
-- Rules invented by Torben Mogensen.
--
-- This is a variant where only orthogonal neighbours are considered.
--
-- ==
-- tags { notravis }

fun i8 sum_of_cell_and_neighbors(int i, int j, [[i8,m],n] board) =
  unsafe
  let above = (i - 1) % n in
  let below = (i + 1) % n in
  let right = (j + 1) % m in
  let left = (j - 1) % m in
  board[above,j] +
  board[i,left] + board[i,j] + board[i,right] +
  board[below,j]

fun [[i8,m],n] all_neighbour_sums([[i8,m],n] board) =
  map(fn [i8,m] (int i) =>
        map(fn i8 (int j) =>
              sum_of_cell_and_neighbors(i,j,board)
           , iota(m))
     , iota(n))

fun [[i8,m],n] iteration([[i8,m],n] board) =
  let all_sums = all_neighbour_sums(board) in
  map(fn [i8,m] ([i8] row_sums) =>
        map(fn i8 (i8 s) =>
              let t = [0i8, 1i8, 1i8, 0i8,
                       0i8, 1i8, 1i8, 1i8,
                       2i8, 2i8, 2i8, 3i8,
                       3i8, 2i8, 2i8, 3i8]
              in unsafe t[int(s)]
           , row_sums)
     , all_sums)

fun int min(int x, int y) =
  if x < y then x else y

entry ([[i8,m],n], [[int,m],n]) init([[bool,m],n] world) =
  (map(fn [i8,m] ([bool] row) =>
         map(fn i8 (bool b) =>
               if b then 1i8 else 0i8,
             row),
         world),
   replicate(n, replicate(m, 0 << 2)))

entry [[[i8,3],m],n] render_frame([[int,m],n] all_history) =
  map(fn [[i8,3],m] ([int] row_history) =>
        map(colour_history, row_history)
     , all_history)

fun [i8,3] colour_history(int history) =
  let used_to_be = history & 3 -- Last two bits encode the previous live cell.
  let age = min(255, history >> 2)
  let colours = [[0i8,   0i8,   255i8],
                 [0i8,   255i8, 0i8],
                 [255i8, 0i8,   0i8],
                 [255i8, 255i8, 0i8]]
  let colour = unsafe colours[used_to_be]
  in map(-i8(age), colour)

fun int update_history(int history, i8 now) =
  let used_to_be = history & 3 -- Last two bits encode the previous live cell.
  let age = min(128, history >> 2)
  in if now == i8(used_to_be)
     then (((age + 1) << 2) | int(now))
     else int(now)

entry ([[i8,m],n], [[int,m],n])
  steps([[i8,m],n] world, [[int,m],n] history, int steps) =
  loop ((world, history)) = for i < steps do
    (let world' = iteration(world)
     let history' = zipWith(fn [int,m] ([int] row_history, [i8] row) =>
                              zipWith(update_history, row_history, row),
                            history, world')
     in (world', history'))
  in (world, history)

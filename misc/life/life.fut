-- Simple game of life implementation with a donut world.

fun bint(b: bool): i32 = if b then 1 else 0
fun intb(x: i32): bool = if x == 0 then false else true

fun to_bool_board(board: [][]i32): [][]bool =
  map (\(r: []i32): []bool  -> map intb r) board

fun to_int_board(board: [][]bool): [][]i32 =
  map (\(r: []bool): []i32  -> map bint r) board

fun cell_neighbors(i: i32, j: i32, board: [n][m]bool): i32 =
  unsafe
  let above = (i - 1) % n
  let below = (i + 1) % n
  let right = (j + 1) % m
  let left = (j - 1) % m
  in
  bint(board[above,left]) + bint(board[above,j]) + bint(board[above,right]) +
  bint(board[i,left]) + bint(board[i,right]) +
  bint(board[below,left]) + bint(board[below,j]) + bint(board[below,right])

fun all_neighbours(board: [n][m]bool): [n][m]i32 =
  map (\(i: i32): []i32  ->
        map (\(j: i32): i32  ->
              cell_neighbors(i,j,board)
           ) (iota(m))
     ) (iota(n))

fun iteration(board: [n][m]bool): [n][m]bool =
  let lives = all_neighbours(board)
  in map (\(lives_r: []i32) (board_r: []bool): []bool  ->
                map (\(neighbors: i32) (alive: bool): bool  ->
                           if neighbors < 2
                           then false
                           else if neighbors == 3 then true
                           else if alive && neighbors < 4 then true
                           else false
                        ) (lives_r) (board_r)
             ) lives board

fun min(x: i32, y: i32): i32 =
  if x < y then x else y


entry init(world: [n][m]bool): ([n][m]bool, [n][m]i32) =
  (world, replicate n (replicate m 255))

entry render_frame(history: [n][m]i32): [n][m][3]i8 =
  map (\(ages: []i32): [m][3]i8  ->
        map (\(age: i32): [3]i8  ->
              if age == 0
              then [0i8, 0i8, 0i8]
              else let c = 127i8 + i8(min(age,127))
                   in [255i8, c, c]
           ) ages
     ) history


entry steps(world: [n][m]bool, history: [n][m]i32, steps: i32): ([n][m]bool, [n][m]i32) =
  loop ((world, history)) = for i < steps do
    (let world' = iteration(world)
     let history' = map (\(xs: []i32) (alives: []bool): [m]i32  ->
                              map (\(x: i32) (alive: bool): i32  ->
                                        if alive then 0 else x + 1) xs alives) history (world')
     in (world', history'))
  in (world, history)

fun main(): i32 = 2

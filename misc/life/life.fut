-- Simple game of life implementation with a donut world.

fun bint(b: bool): int = if b then 1 else 0
fun intb(x: int): bool = if x == 0 then false else true

fun to_bool_board(board: [][]int): [][]bool =
  map (fn (r: []int): []bool  => map intb r) board

fun to_int_board(board: [][]bool): [][]int =
  map (fn (r: []bool): []int  => map bint r) board

fun cell_neighbors(i: int, j: int, board: [n][m]bool): int =
  unsafe
  let above = (i - 1) % n
  let below = (i + 1) % n
  let right = (j + 1) % m
  let left = (j - 1) % m
  in
  bint(board[above,left]) + bint(board[above,j]) + bint(board[above,right]) +
  bint(board[i,left]) + bint(board[i,right]) +
  bint(board[below,left]) + bint(board[below,j]) + bint(board[below,right])

fun all_neighbours(board: [n][m]bool): [n][m]int =
  map (fn (i: int): []int  =>
        map (fn (j: int): int  =>
              cell_neighbors(i,j,board)
           ) (iota(m))
     ) (iota(n))

fun iteration(board: [n][m]bool): [n][m]bool =
  let lives = all_neighbours(board)
  in map (fn (lives_r: []int) (board_r: []bool): []bool  =>
                map (fn (neighbors: int) (alive: bool): bool  =>
                           if neighbors < 2
                           then false
                           else if neighbors == 3 then true
                           else if alive && neighbors < 4 then true
                           else false
                        ) (lives_r) (board_r)
             ) lives board

fun min(x: int, y: int): int =
  if x < y then x else y


entry init(world: [n][m]bool): ([n][m]bool, [n][m]int) =
  (world, replicate n (replicate m 255))

entry render_frame(history: [n][m]int): [n][m][3]i8 =
  map (fn (ages: []int): [m][3]i8  =>
        map (fn (age: int): [3]i8  =>
              if age == 0
              then [0i8, 0i8, 0i8]
              else let c = 127i8 + i8(min(age,127))
                   in [255i8, c, c]
           ) ages
     ) history


entry steps(world: [n][m]bool, history: [n][m]int, steps: int): ([n][m]bool, [n][m]int) =
  loop ((world, history)) = for i < steps do
    (let world' = iteration(world)
     let history' = map (fn (xs: []int) (alives: []bool): [m]int  =>
                              map (fn (x: int) (alive: bool): int  =>
                                        if alive then 0 else x + 1) xs alives) history (world')
     in (world', history'))
  in (world, history)

fun main(): int = 2

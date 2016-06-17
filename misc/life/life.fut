-- Simple game of life implementation with a donut world.

fun int bint(bool b) = if b then 1 else 0
fun bool intb(int x) = if x == 0 then False else True

fun [][]bool to_bool_board([][]int board) =
  map(fn []bool ([]int r) => map(intb, r), board)

fun [][]int to_int_board([][]bool board) =
  map(fn []int ([]bool r) => map(bint, r), board)

fun int cell_neighbors(int i, int j, [n][m]bool board) =
  unsafe
  let above = (i - 1) % n in
  let below = (i + 1) % n in
  let right = (j + 1) % m in
  let left = (j - 1) % m in
  bint(board[above,left]) + bint(board[above,j]) + bint(board[above,right]) +
  bint(board[i,left]) + bint(board[i,right]) +
  bint(board[below,left]) + bint(board[below,j]) + bint(board[below,right])

fun [n][m]int all_neighbours([n][m]bool board) =
  map(fn []int (int i) =>
        map(fn int (int j) =>
              cell_neighbors(i,j,board)
           , iota(m))
     , iota(n))

fun [n][m]bool iteration([n][m]bool board) =
  let lives = all_neighbours(board) in
  zipWith(fn []bool ([]int lives_r, []bool board_r) =>
            zipWith(fn bool (int neighbors, bool alive) =>
                      if neighbors < 2
                      then False
                      else if neighbors == 3 then True
                      else if alive && neighbors < 4 then True
                                                     else False
                   , lives_r, board_r)
         , lives, board)

fun int min(int x, int y) =
  if x < y then x else y


entry ([n][m]bool, [n][m]int) init([n][m]bool world) =
  (world, replicate(n, replicate(m, 255)))

entry [n][m][3]i8 render_frame([n][m]int history) =
  map(fn [m][3]i8 ([]int ages) =>
        map(fn [3]i8 (int age) =>
              if age == 0
              then [0i8, 0i8, 0i8]
              else let c = 127i8 + i8(min(age,127))
                   in [255i8, c, c]
           , ages)
     , history)


entry ([n][m]bool, [n][m]int)
  steps([n][m]bool world, [n][m]int history, int steps) =
  loop ((world, history)) = for i < steps do
    (let world' = iteration(world)
     let history' = zipWith(fn [m]int ([]int xs, []bool alives) =>
                              zipWith(fn int (int x, bool alive) =>
                                        if alive then 0 else x + 1,
                                      xs, alives),
                              history, world')
     in (world', history'))
  in (world, history)

fun int main() = 2

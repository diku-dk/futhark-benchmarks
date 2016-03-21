-- Simple game of life implementation with a donut world.

fun int bint(bool b) = if b then 1 else 0
fun bool intb(int x) = if x == 0 then False else True

fun [[bool]] to_bool_board([[int]] board) =
  map(fn [bool] ([int] r) => map(intb, r), board)

fun [[int]] to_int_board([[bool]] board) =
  map(fn [int] ([bool] r) => map(bint, r), board)

fun int cell_neighbors(int i, int j, [[bool,m],n] board) =
  unsafe
  let above = (i - 1) % n in
  let below = (i + 1) % n in
  let right = (j + 1) % m in
  let left = (j - 1) % m in
  bint(board[above,left]) + bint(board[above,j]) + bint(board[above,right]) +
  bint(board[i,left]) + bint(board[i,right]) +
  bint(board[below,left]) + bint(board[below,j]) + bint(board[below,right])

fun [[int,m],n] all_neighbours([[bool,m],n] board) =
  map(fn [int] (int i) =>
        map(fn int (int j) =>
              cell_neighbors(i,j,board)
           , iota(m))
     , iota(n))

fun [[bool,m],n] iteration([[bool,m],n] board) =
  let lives = all_neighbours(board) in
  zipWith(fn [bool] ([int] lives_r, [bool] board_r) =>
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


entry {[[bool,m],n], [[int,m],n]} init([[bool,m],n] world) =
  {world, replicate(n, replicate(m, 255))}

entry [[int,m],n] render_frame([[int,m],n] history) =
  map(fn [int,m] ([int] ages) =>
        map(fn int (int age) =>
              if age == 0
              then 0
              else let c = 128 + min(127, age)
                   in c << 16 | c << 8 | c
           , ages)
     , history)


entry {[[bool,m],n], [[int,m],n]}
  steps([[bool,m],n] world, [[int,m],n] history, int steps) =
  loop ({world, history}) = for i < steps do
    (let world' = iteration(world)
     let history' = zipWith(fn [int,m] ([int] xs, [bool] alives) =>
                              zipWith(fn int (int x, bool alive) =>
                                        if alive then 0 else x + 1,
                                      xs, alives),
                              history, world')
     in {world', history'})
  in {world, history}

fun int main() = 2

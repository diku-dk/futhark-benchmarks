-- Buddhabrot fractal: https://en.wikipedia.org/wiki/Buddhabrot

default(f32)

fun f32 dot((f32,f32) c) =
  let (r, i) = c
  in r * r + i * i

fun (f32,f32) multComplex((f32,f32) x, (f32,f32) y) =
  let (a, b) = x
  let (c, d) = y
  in (a*c - b * d,
      a*d + b * c)

fun (f32,f32) addComplex((f32,f32) x, (f32,f32) y) =
  let (a, b) = x
  let (c, d) = y
  in (a + c,
      b + d)

fun ([depth](f32,f32),bool) divergence(int depth, (f32,f32) c0) =
  let trajectory = replicate(depth, (0.0, 0.0))
  loop ((trajectory, c, i) = (trajectory, c0, 0)) = while i < depth && dot(c) < 4.0 do
    unsafe
    let c' = addComplex(c0, multComplex(c, c))
    let trajectory[i] = c'
    in (trajectory, c', i + 1)
    in (trajectory, i == depth)

fun ([yprec][xprec][depth](f32,f32),
     [yprec][xprec]bool) trajectories(int depth, int xprec, int yprec,
                                        (f32,f32,f32,f32) field) =
  let (xmin, ymin, xmax, ymax) = field
  let sizex = xmax - xmin
  let sizey = ymax - ymin
  let (trajectories, escapes) =
    unzip(map (fn ([depth](f32,f32),bool) (int i) =>
                 let x = i % xprec
                 let y = i / yprec
                 let c0 = (xmin + (f32(x) * sizex) / f32(xprec),
                           ymin + (f32(y) * sizey) / f32(yprec))
                 in divergence(depth, c0)
              , iota(xprec*yprec)))
  in (reshape((yprec,xprec,depth), trajectories),
      reshape((xprec,yprec), escapes))

fun int toI(int n, (f32,f32,f32,f32) view, f32 y) =
  let (xmin, ymin, xmax, ymax) = view
  let sizey = ymax - ymin
  let y' = y - ymin
  in int(y' / (sizey / f32(n)))

fun int toJ(int m, (f32,f32,f32,f32) view, f32 x) =
  let (xmin, ymin, xmax, ymax) = view
  let sizex = xmax - xmin
  let x' = x - xmin
  in int(x' / (sizex / f32(m)))

fun int max(int x, int y) =
  if x < y then y else x

fun int colourise(int max_visits, int visits) =
  let c = 255-int(log32(f32(visits)) / log32(f32(max_visits)) * 255.0)
  in c << 16 | c << 8 | c

fun [n][m]int visualise(int n, int m, (f32,f32,f32,f32) view,
                          [yprec][xprec][depth](f32,f32) trajectories,
                          [yprec][xprec]bool escapes) =
  let (xmin, ymin, xmax, ymax) = view
  let sizex = xmax - xmin
  let sizey = ymax - ymin
  let trajectories' = reshape((xprec*yprec,depth), trajectories)
  let escapes' = reshape((xprec*yprec), escapes)
  let visits_per_pixel =
    reshape((n*m),
            streamRedPer(fn [n][m]int ([n][m]int ass, [n][m]int bss) =>
                           zipWith(fn [m]int ([m]int as, [m]int bs) =>
                                     zipWith(+, as, bs),
                                   ass, bss),
                           fn [n][m]int (int chunk,
                                           *[n][m]int acc,
                                           []([depth](f32,f32),bool) inp) =>
                             loop (acc) = for i < chunk do
                               (let (trajectory, escaped) = inp[i]
                                in if escaped then (loop (acc) = for j < depth do
                                                      (unsafe
                                                       let (x,y) = trajectory[j]
                                                       let i = toI(n, view, y)
                                                       let j = toJ(m, view, x)
                                                       in if i >= 0 && i < n && j >= 0 && j < m
                                                          then let acc[i,j] = acc[i,j] + 1
                                                               in acc
                                                          else acc)
                                                    in acc)
                                              else acc)
                             in acc,
                         replicate(n, replicate(m, 0)), zip(trajectories', escapes')))
  let max_visits = reduce(max, 0, reshape((n*m), visits_per_pixel))
  let coloured = map(colourise(max_visits), visits_per_pixel)
  in reshape((n,m), coloured)

fun [n][m]int main(int n, int m, f32 v_xmin, f32 v_ymin, f32 v_xmax, f32 v_ymax,
                     int depth,
                     int xprec, int yprec, f32 f_xmin, f32 f_ymin, f32 f_xmax, f32 f_ymax) =
  let (trajectories, escapes) = trajectories(depth, xprec, yprec,
                                             (f_xmin, f_ymin, f_xmax, f_ymax))
  let image = visualise(n, m, (v_xmin, v_ymin, v_xmax, v_ymax), trajectories, escapes)
  in image

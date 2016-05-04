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

fun ([(f32,f32),depth],bool) divergence(int depth, (f32,f32) c0) =
  let trajectory = replicate(depth, (0.0, 0.0))
  loop ((trajectory, c, i) = (trajectory, c0, 0)) = while i < depth && dot(c) < 4.0 do
    unsafe
    let c' = addComplex(c0, multComplex(c, c))
    let trajectory[i] = c'
    in (trajectory, c', i + 1)
    in (trajectory, i == depth)

fun ([[[(f32,f32),depth],xprec],yprec],
     [[bool,xprec],yprec]) trajectories(int depth, int xprec, int yprec,
                                        (f32,f32,f32,f32) field) =
  let (xmin, ymin, xmax, ymax) = field
  let sizex = xmax - xmin
  let sizey = ymax - ymin
  let (trajectories, escapes) =
    unzip(map (fn ([(f32,f32),depth],bool) (int i) =>
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

fun [[int,m],n] visualise(int n, int m, (f32,f32,f32,f32) view,
                          [[[(f32,f32),depth],xprec],yprec] trajectories,
                          [[bool,xprec],yprec] escapes) =
  let (xmin, ymin, xmax, ymax) = view
  let sizex = xmax - xmin
  let sizey = ymax - ymin
  let trajectories' = reshape((xprec*yprec,depth), trajectories)
  let escapes' = reshape((xprec*yprec), escapes)
  let visits_per_pixel =
    reshape((n*m),
            streamRedPer(fn [[int,m],n] ([[int,m],n] ass, [[int,m],n] bss) =>
                           zipWith(fn [int,m] ([int,m] as, [int,m] bs) =>
                                     zipWith(+, as, bs),
                                   ass, bss),
                           fn [[int,m],n] (int chunk,
                                           *[[int,m],n] acc,
                                           [([(f32,f32),depth],bool)] inp) =>
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

fun [[int,m],n] main(int n, int m, (f32,f32,f32,f32) view,
                     int depth,
                     int xprec, int yprec, (f32,f32,f32,f32) field) =
  let (trajectories, escapes) = trajectories(depth, xprec, yprec, field)
  let image = visualise(n, m, view, trajectories, escapes)
  in image

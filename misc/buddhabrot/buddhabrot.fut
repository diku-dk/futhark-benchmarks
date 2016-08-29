-- Buddhabrot fractal: https://en.wikipedia.org/wiki/Buddhabrot

default(f32)

fun dot(c: (f32,f32)): f32 =
  let (r, i) = c
  in r * r + i * i

fun multComplex(x: (f32,f32), y: (f32,f32)): (f32,f32) =
  let (a, b) = x
  let (c, d) = y
  in (a*c - b * d,
      a*d + b * c)

fun addComplex(x: (f32,f32), y: (f32,f32)): (f32,f32) =
  let (a, b) = x
  let (c, d) = y
  in (a + c,
      b + d)

fun divergence(depth: int, c0: (f32,f32)): ([depth](f32,f32),bool) =
  let trajectory = replicate depth (0.0, 0.0)
  loop ((trajectory, c, i) = (trajectory, c0, 0)) = while i < depth && dot(c) < 4.0 do
    unsafe
    let c' = addComplex(c0, multComplex(c, c))
    let trajectory[i] = c'
    in (trajectory, c', i + 1)
    in (trajectory, i == depth)

fun trajectories(depth: int, xprec: int, yprec: int,
                                        field: (f32,f32,f32,f32)): ([yprec][xprec][depth](f32,f32),
     [yprec][xprec]bool) =
  let (xmin, ymin, xmax, ymax) = field
  let sizex = xmax - xmin
  let sizey = ymax - ymin
  let (trajectories, escapes) =
    unzip(map  (fn (i: int): ([depth](f32,f32),bool)  =>
                 let x = i % xprec
                 let y = i / yprec
                 let c0 = (xmin + (f32(x) * sizex) / f32(xprec),
                           ymin + (f32(y) * sizey) / f32(yprec))
                 in divergence(depth, c0)
              ) (iota(xprec*yprec)))
  in (reshape (yprec,xprec,depth) trajectories,
      reshape (xprec,yprec) escapes)

fun toI(n: int, view: (f32,f32,f32,f32), y: f32): int =
  let (xmin, ymin, xmax, ymax) = view
  let sizey = ymax - ymin
  let y' = y - ymin
  in int(y' / (sizey / f32(n)))

fun toJ(m: int, view: (f32,f32,f32,f32), x: f32): int =
  let (xmin, ymin, xmax, ymax) = view
  let sizex = xmax - xmin
  let x' = x - xmin
  in int(x' / (sizex / f32(m)))

fun max(x: int) (y: int): int =
  if x < y then y else x

fun colourise(max_visits: int) (visits: int): int =
  let c = 255-int(log32(f32(visits)) / log32(f32(max_visits)) * 255.0)
  in c << 16 | c << 8 | c

fun visualise(n: int, m: int, view: (f32,f32,f32,f32),
                          trajectories: [yprec][xprec][depth](f32,f32),
                          escapes: [yprec][xprec]bool): [n][m]int =
  let (xmin, ymin, xmax, ymax) = view
  let sizex = xmax - xmin
  let sizey = ymax - ymin
  let trajectories' = reshape (xprec*yprec,depth) trajectories
  let escapes' = reshape (xprec*yprec) escapes
  let visits_per_pixel =
    reshape (n*m)
            (streamRedPer (fn (ass: [n][m]int) (bss: [n][m]int): [n][m]int  =>
                           zipWith (fn (as: [m]int, bs: [m]int): [m]int  =>
                                     zipWith (+) as bs) ass bss) (
                         fn (chunk: int)
                            (acc: *[n][m]int)
                            (inp: []([depth](f32,f32),bool)): [n][m]int  =>
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
                             in acc) (
                         replicate n (replicate m 0)) (zip (trajectories') (escapes')))
  let max_visits = reduce max 0 (reshape (n*m) visits_per_pixel)
  let coloured = map (colourise(max_visits)) (visits_per_pixel)
  in reshape (n,m) coloured

fun main(n: int, m: int, v_xmin: f32, v_ymin: f32, v_xmax: f32, v_ymax: f32,
                     depth: int,
                     xprec: int, yprec: int, f_xmin: f32, f_ymin: f32, f_xmax: f32, f_ymax: f32): [n][m]int =
  let (trajectories, escapes) = trajectories(depth, xprec, yprec,
                                             (f_xmin, f_ymin, f_xmax, f_ymax))
  let image = visualise(n, m, (v_xmin, v_ymin, v_xmax, v_ymax), trajectories, escapes)
  in image

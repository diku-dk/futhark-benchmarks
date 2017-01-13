-- Many of these names are taken directly from srad_v1 in the Rodinia
-- suite.  It looks like some Fortran remnant (sorry, I mean
-- "FORTRAN").
--
-- Some comments are also from original source code.
--
-- One annoying difference is that the original program assumes
-- column-major storage, whereas we use row-major storage here.
--
-- The original program rounds the final result to integers (as is
-- proper for an image).  This creates problems with getting the same
-- results on GPU and CPU, as differences in floating-point accuracy
-- leads to slight changes that just might push the rounded integer up
-- or down, thus exaggerating the difference.  For simplicity, this
-- mplementation returns floats instead.  This should have no
-- measurable impact on performance, as we still perform the scaling
-- to (0,255).
--
-- ==
-- compiled input @ data/image.in
-- output @ data/image.out

default(f32)

fun indexN(_rows: int, i: int): int =
  if i == 0 then i else i - 1

fun indexS(rows: int, i: int): int =
  if i == rows-1 then i else i + 1

fun indexW(_cols: int, j: int): int =
  if j == 0 then j else j - 1

fun indexE(cols: int, j: int): int =
  if j == cols-1 then j else j + 1

fun do_srad(niter: int, lambda: f32, image: [rows][cols]u8): [rows][cols]f32 =
  let r1 = 0
  let r2 = rows - 1
  let c1 = 0
  let c2 = cols - 1
  let ne = rows * cols

  -- ROI image size
  let neROI = (r2-r1+1)*(c2-c1+1)

  -- SCALE IMAGE DOWN FROM 0-255 TO 0-1 AND EXTRACT
  let image = map (\(row: []u8): [cols]f32  ->
                    map (\(pixel: u8): f32  ->
                          exp32(f32(pixel)/255.0)) row) image
  loop (image) = for _i < niter do
    -- ROI statistics for entire ROI (single number for ROI)
    let sum = reduce (+) (0.0) (reshape (ne) image)
    let sum2 = reduce (+) (0.0) (map (**2.0) (reshape (ne) image))
    -- get mean (average) value of element in ROI
    let meanROI = sum / f32(neROI)
    -- gets variance of ROI
    let varROI = (sum2 / f32(neROI)) - meanROI*meanROI
    -- gets standard deviation of ROI
    let q0sqr = varROI / (meanROI*meanROI)

    let (dN, dS, dW, dE, c) =
      unzip(
        map (\(i: int) (row: []f32): [cols](f32,f32,f32,f32,f32)
                   ->
                    map (\(j: int) (jc: f32): (f32,f32,f32,f32,f32)  ->
                              let dN_k = unsafe image[indexN(rows,i),j] - jc
                              let dS_k = unsafe image[indexS(rows,i),j] - jc
                              let dW_k = unsafe image[i, indexW(cols,j)] - jc
                              let dE_k = unsafe image[i, indexE(cols,j)] - jc
                              let g2 = (dN_k*dN_k + dS_k*dS_k +
                                        dW_k*dW_k + dE_k*dE_k) / (jc*jc)
                              let l = (dN_k + dS_k + dW_k + dE_k) / jc
                              let num = (0.5*g2) - ((1.0/16.0)*(l*l))
                              let den = 1.0 + 0.25*l
                              let qsqr = num / (den*den)
                              let den = (qsqr-q0sqr) / (q0sqr * (1.0+q0sqr))
                              let c_k = 1.0 / (1.0+den)
                              let c_k = if c_k < 0.0
                                        then 0.0
                                        else if c_k > 1.0
                                             then 1.0 else c_k
                              in (dN_k, dS_k, dW_k, dE_k, c_k)
                           ) (iota(cols)) row
               ) (iota(rows)) image)

    let image =
      map (\i image_row c_row dN_row dS_row dW_row dE_row: [cols]f32 ->
                map (\j pixel c_k dN_k dS_k dW_k dE_k  ->
                          let cN = c_k
                          let cS = unsafe c[indexS(rows, i), j]
                          let cW = c_k
                          let cE = unsafe c[i, indexE(cols,j)]
                          let d = cN*dN_k + cS*dS_k + cW*dW_k + cE*dE_k
                          in pixel + 0.25 * lambda * d)
                        (iota cols) image_row c_row dN_row dS_row dW_row dE_row)
                        (iota rows) image c dN dS dW dE
    in image

  -- SCALE IMAGE UP FROM 0-1 TO 0-255 AND COMPRESS
  let image = map (\(row: []f32): [cols]f32  ->
                    map (\(pixel: f32): f32  ->
                          -- take logarithm of image (log compress).
                          -- This is where the original implementation
                          -- would round to int.
                           log32(pixel)*255.0) row) image
  in image

fun main(image: [rows][cols]u8): [rows][cols]f32 =
  let niter = 100
  let lambda = 0.5
  in do_srad(niter, lambda, image)

-- Entry point for interactive demo.  Here we can return an RGBA image.
entry srad(niter: int, lambda: f32, image: [rows][cols]u8): [rows][cols]int =
  map (\row -> map (\p -> (int(p) << 16) | (int(p) << 8) | (int(p))) row)
      (do_srad(niter, lambda, image))

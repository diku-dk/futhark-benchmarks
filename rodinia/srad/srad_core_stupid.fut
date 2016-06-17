-- Like srad_core, but manifests stencil index arrays like the
-- original srad_v1 from Rodinia.  This is kept around as a test case
-- for fusion - it is 25% slower than srad_core.fut.
--
-- ==
-- notravis input @ data/image.in
-- output @ data/image.out

default(f32)

fun [rows][cols]int main([rows][cols]int image) =
  let niter = 100 in
  let lambda = 0.5 in
  let r1 = 0 in
  let r2 = rows - 1 in
  let c1 = 0 in
  let c2 = cols - 1 in
  let ne = rows * cols in

  -- ROI image size
  let neROI = (r2-r1+1)*(c2-c1+1) in

  -- N/S/W/E indices of surrounding pixels (every element of IMAGE)
  -- holds index of IMAGE row above
  let iN = map(-1, iota(rows)) in
  -- holds index of IMAGE row below
  let iS = map(+1, iota(rows)) in
  -- holds index of IMAGE column on the left
  let jW = map(-1, iota(cols)) in
  -- holds index of IMAGE column on the right
  let jE = map(+1, iota(cols)) in

  -- N/S/W/E boundary conditions, fix surrounding indices outside boundary of IMAGE
  let iN[0] = 0 in
  let iS[rows-1] = rows - 1 in
  let jW[0] = 0 in
  let jE[cols-1] = cols - 1 in

  -- SCALE IMAGE DOWN FROM 0-255 TO 0-1 AND EXTRACT
  let image = map(fn [cols]f32 ([]int row) =>
                    map(fn f32 (int pixel) =>
                          exp32(f32(pixel)/255.0),
                        row),
                    image) in
  loop (image) = for i < niter do
    -- ROI statistics for entire ROI (single number for ROI)
    let sum = reduce(+, 0.0, reshape((ne), image)) in
    let sum2 = reduce(+, 0.0, map(**2.0, reshape((ne), image))) in
    -- get mean (average) value of element in ROI
    let meanROI = sum / f32(neROI) in
    -- gets variance of ROI
    let varROI = (sum2 / f32(neROI)) - meanROI*meanROI in
    -- gets standard deviation of ROI
    let q0sqr = varROI / (meanROI*meanROI) in

    let (dN, dS, dW, dE, c) =
      unzip(
        zipWith(fn [cols](f32,f32,f32,f32,f32)
                  (int i, []f32 row) =>
                    zipWith(fn (f32,f32,f32,f32,f32) (int j, f32 jc) =>
                              let dN_k = image[iN[i],j] - jc in
                              let dS_k = image[iS[i],j] - jc in
                              let dW_k = image[i, jW[j]] - jc in
                              let dE_k = image[i, jE[j]] - jc in
                              let g2 = (dN_k*dN_k + dS_k*dS_k +
                                        dW_k*dW_k + dE_k*dE_k) / (jc*jc) in
                              let l = (dN_k + dS_k + dW_k + dE_k) / jc in
                              let num = (0.5*g2) - ((1.0/16.0)*(l*l)) in
                              let den = 1.0 + 0.25*l in
                              let qsqr = num / (den*den) in
                              let den = (qsqr-q0sqr) / (q0sqr * (1.0+q0sqr)) in
                              let c_k = 1.0 / (1.0+den) in
                              let c_k = if c_k < 0.0
                                        then 0.0
                                        else if c_k > 1.0
                                             then 1.0 else c_k in
                              (dN_k, dS_k, dW_k, dE_k, c_k)
                           , iota(cols), row)
               , iota(rows), image)) in

    let image =
      zipWith(fn [cols]f32
                (int i, []f32 image_row, []f32 c_row, []f32 dN_row, []f32 dS_row, []f32 dW_row, []f32 dE_row) =>
                zipWith(fn f32 (int j, f32 pixel, f32 c_k, f32 dN_k, f32 dS_k, f32 dW_k, f32 dE_k) =>
                          let cN = c_k in
                          let cS = c[iS[i], j] in
                          let cW = c_k in
                          let cE = c[i, jE[j]] in
                          let d = cN*dN_k + cS*dS_k + cW*dW_k + cE*dE_k in
                          pixel + 0.25 * lambda * d
                       , iota(cols), image_row, c_row, dN_row, dS_row, dW_row, dE_row),
                iota(rows), image, c, dN, dS, dW, dE) in
    image in

  -- SCALE IMAGE UP FROM 0-1 TO 0-255 AND COMPRESS
  let image = map(fn [cols]int ([]f32 row) =>
                    map(fn int (f32 pixel) =>
                          -- take logarithm of image, log compress
                          int(log32(pixel)*255.0),
                        row),
                    image) in
  image

-- Code and comments based on
-- https://github.com/kkushagra/rodinia/blob/master/openmp/hotspot/hotspot_openmp.cpp
--
-- ==
-- tags { futhark-c futhark-opencl }
-- compiled input @ data/64.in
-- output @ data/64.out
--
-- compiled input @ data/512.in
-- output @ data/512.out
--
-- notravis input @ data/1024.in
-- output @ data/1024.out

default(f32)

fun int str_size() = 256

-- Maximum power density possible (say 300W for a 10mm x 10mm chip)
fun f32 max_pd() = 3.0e6

-- Required precision in degrees
fun f32 precision() = 0.001

fun f32 spec_heat_si() = 1.75e6

fun f32 k_si() = 100.0

-- Capacitance fitting factor
fun f32 factor_chip() = 0.5

-- Chip parameters
fun f32 t_chip() = 0.0005
fun f32 chip_height() = 0.016
fun f32 chip_width() = 0.016

-- Ambient temperature assuming no package at all
fun f32 amb_temp() = 80.0

-- Single iteration of the transient solver in the grid model.
-- advances the solution of the discretized difference equations by
-- one time step
fun [[f32]] single_iteration([[f32,col],row] temp, [[f32,col],row] power,
                              f32 Cap, f32 Rx, f32 Ry, f32 Rz,
                              f32 step) =
  map (fn [f32] (int r) =>
         map(fn f32 (int c) =>
               let delta =
                 (step / Cap) *
               (power[r,c] +
                unsafe
                  (if r == 0 && c == 0 then -- Corner 1
                     (temp[r,c+1] - temp[r,c]) / Rx +
                     (temp[r+1,c] - temp[r,c]) / Ry
                   else if r == 0 && c == col-1 then -- Corner 2
                     (temp[r,c-1] - temp[r,c]) / Rx +
                     (temp[r+1,c] - temp[r,c]) / Ry
                   else if r == row-1 && c == col-1 then -- Corner 3
                     (temp[r,c-1] - temp[r,c]) / Rx +
                     (temp[r-1,c] - temp[r,c]) / Ry
                   else if r == row-1 && c == 0 then -- Corner 4
                     (temp[r,c+1] - temp[r,c]) / Rx +
                     (temp[r-1,c] - temp[r,c]) / Ry
                   else if r == 0 then -- Edge 1
                     (temp[r,c+1] + temp[r,c-1] - 2.0*temp[r,c]) / Rx +
                     (temp[r+1,c] - temp[r,c]) / Ry
                   else if c == col-1 then -- Edge 2
                     (temp[r,c-1] - temp[r,c]) / Rx +
                     (temp[r+1,c] + temp[r-1,c] - 2.0*temp[r,c]) / Ry
                   else if r == row-1 then -- Edge 3
                     (temp[r,c+1] + temp[r,c-1] - 2.0*temp[r,c]) / Rx +
                     (temp[r-1,c] - temp[r,c]) / Ry
                   else if c == 0 then -- Edge 4
                     (temp[r,c+1] - temp[r,c]) / Rx +
                     (temp[r+1,c] + temp[r-1,c] - 2.0*temp[r,c]) / Ry
                   else
                     (temp[r,c+1] + temp[r,c-1] - 2.0 * temp[r,c]) / Rx +
                     (temp[r+1,c] + temp[r-1,c] - 2.0 * temp[r,c]) / Ry) +
                  (amb_temp() - temp[r,c]) / Rz) in
               temp[r,c] + delta
            , iota(col)),
         iota(row))

-- Transient solver driver routine: simply converts the heat transfer
-- differential equations to difference equations and solves the
-- difference equations by iterating
fun [[f32]] compute_tran_temp(int num_iterations, [[f32,col],row] temp, [[f32,col],row] power) =
  let grid_height = chip_height() / f32(row) in
  let grid_width = chip_width() / f32(col) in
  let Cap = factor_chip() * spec_heat_si() * t_chip() * grid_width * grid_height in
  let Rx = grid_width / (2.0 * k_si() * t_chip() * grid_height) in
  let Ry = grid_height / (2.0 * k_si() * t_chip() * grid_width) in
  let Rz = t_chip() / (k_si() * grid_height * grid_width) in
  let max_slope = max_pd() / (factor_chip() * t_chip() * spec_heat_si()) in
  let step = precision() / max_slope in
  loop (temp) = for i < num_iterations do
    single_iteration(temp, power, Cap, Rx, Ry, Rz, step) in
  temp

fun [[f32]] main(int num_iterations, [[f32,col],row] temp, [[f32,col],row] power) =
  compute_tran_temp(num_iterations, temp, power)

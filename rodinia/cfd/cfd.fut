-- Code and comments based on
-- https://github.com/kkushagra/rodinia/blob/master/openmp/cfd/euler3d_cpu.cpp
--
-- ==
--
-- notravis input @ data/fvcorr.domn.097K.toa
-- output @ data/fvcorr.domn.097K.out

-- notravis input @ data/fvcorr.domn.097K.toa
-- output @ data/fvcorr.domn.097K.5its.out

-- The other datasets (not for now...)
-- compiled input @ data/fvcorr.domn.193K.toa
-- output @ data/fvcorr.domn.193K.out
--
-- compiled input @ data/missile.domn.0.2M.toa
-- output @ data/missile.domn.0.2M.out

default(f32)

fun f32 GAMMA() = 1.4
fun int  iterations() = 1300 --2000

--#define NDIM 3
--#define NNB 4

fun int  RK() = 3	-- 3rd order RK
fun f32 ff_mach() = 1.2
fun f32 deg_angle_of_attack() = 0.0

-- not options
fun int  VAR_DENSITY() = 0
fun int  VAR_MOMENTUM()= 1
fun int  VAR_DENSITY_ENERGY() = VAR_MOMENTUM() + 3 --VAR_MOMENTUM+NDIM
fun int  NVAR() = VAR_DENSITY_ENERGY() + 1

-- short functions
fun {f32,f32,f32} compute_velocity(f32 density, {f32,f32,f32} momentum) =
    let {momentum_x, momentum_y, momentum_z} = momentum
    in  {momentum_x / density, momentum_y / density, momentum_z / density}

fun f32 compute_speed_sqd({f32,f32,f32} velocity) = 
    let {velocity_x, velocity_y, velocity_z} = velocity in
    velocity_x*velocity_x + velocity_y*velocity_y + velocity_z*velocity_z

fun f32 compute_pressure(f32 density, f32 density_energy, f32 speed_sqd) = 
    (GAMMA()-1.0) * (density_energy - 0.5*density*speed_sqd)

fun f32 compute_speed_of_sound(f32 density, f32 pressure) =
    sqrt( GAMMA() * pressure / density )

--
fun [[f32,nelr],5] initialize_variables(int nelr, [f32,5] ff_variable) = --[float,NVAR] ff_variable
    map(fn [f32,nelr] (f32 x) => replicate(nelr, x), ff_variable)

-- 
fun {{f32,f32,f32},{f32,f32,f32},{f32,f32,f32},{f32,f32,f32}} 
compute_flux_contribution( f32 density,  {f32,f32,f32} momentum, f32 density_energy, 
                           f32 pressure, {f32,f32,f32} velocity ) =
    let {momentum_x, momentum_y, momentum_z} = momentum in
    let {velocity_x, velocity_y, velocity_z} = velocity in

    let fc_momentum_x_x = velocity_x*momentum_x + pressure in
    let fc_momentum_x_y = velocity_x*momentum_y in
    let fc_momentum_x_z = velocity_x*momentum_z in

    let fc_momentum_y_x = fc_momentum_x_y in
    let fc_momentum_y_y = velocity_y*momentum_y + pressure in
    let fc_momentum_y_z = velocity_y*momentum_z in
    
    let fc_momentum_z_x = fc_momentum_x_z in
    let fc_momentum_z_y = fc_momentum_y_z in
    let fc_momentum_z_z = velocity_z*momentum_z + pressure in
    
    let de_p = density_energy+pressure in
    let fc_density_energy_x = velocity_x*de_p in
    let fc_density_energy_y = velocity_y*de_p in
    let fc_density_energy_z = velocity_z*de_p in

    { {fc_momentum_x_x,fc_momentum_x_y,fc_momentum_x_z}
    , {fc_momentum_y_x,fc_momentum_y_y,fc_momentum_y_z}
    , {fc_momentum_z_x,fc_momentum_z_y,fc_momentum_z_z}
    , {fc_density_energy_x, fc_density_energy_y, fc_density_energy_z}
    }

--
fun [f32,nelr] compute_step_factor([[f32,nelr],5] variables, [f32,nelr] areas) = -- 5 == NVAR
    map(fn f32 (int i) =>
            let density    = variables[VAR_DENSITY(),    i] in
            let momentum_x = variables[VAR_MOMENTUM()+0, i] in
            let momentum_y = variables[VAR_MOMENTUM()+1, i] in
            let momentum_z = variables[VAR_MOMENTUM()+2, i] in
            let momentum   = { momentum_x, momentum_y, momentum_z } in
            let density_energy = variables[VAR_DENSITY_ENERGY(), i] in
            let velocity   = compute_velocity( density, momentum )  in
            let speed_sqd  = compute_speed_sqd(velocity)    in
            let pressure   = compute_pressure(density, density_energy, speed_sqd) in
            let speed_of_sound = compute_speed_of_sound( density, pressure ) in
                ( 0.5 / (sqrt(areas[i]) * (sqrt(speed_sqd) + speed_of_sound) ) )
       , iota(nelr))
    
--5 == NVAR
fun [[f32,nel],5] 
    compute_flux(   [[int,nel],NNB] elements_surrounding_elements
                ,   [[[f32,nel],NNB],NDIM] normals
                ,   [[f32,nel],5]          variables   
                ,   [f32,5]                ff_variable
                ,   {f32,f32,f32}        ff_flux_contribution_momentum_x
                ,   {f32,f32,f32}        ff_flux_contribution_momentum_y
                ,   {f32,f32,f32}        ff_flux_contribution_momentum_z
                ,   {f32,f32,f32}        ff_flux_contribution_density_energy
                ) =
    let { ff_flux_contribution_momentum_x_x, ff_flux_contribution_momentum_x_y, 
          ff_flux_contribution_momentum_x_z } = ff_flux_contribution_momentum_x in
    let { ff_flux_contribution_momentum_y_x, ff_flux_contribution_momentum_y_y,
          ff_flux_contribution_momentum_y_z } = ff_flux_contribution_momentum_y in
    let { ff_flux_contribution_momentum_z_x, ff_flux_contribution_momentum_z_y,
          ff_flux_contribution_momentum_z_z } = ff_flux_contribution_momentum_z in
    let { ff_flux_contribution_density_energy_x, ff_flux_contribution_density_energy_y,
          ff_flux_contribution_density_energy_z } = ff_flux_contribution_density_energy in
    let smoothing_coefficient = 0.2 in
    transpose( 
      map(fn [f32,5] (int i) =>
            let density_i    = variables[VAR_DENSITY(), i]    in
            let momentum_i_x = variables[VAR_MOMENTUM()+0, i] in
            let momentum_i_y = variables[VAR_MOMENTUM()+1, i] in
            let momentum_i_z = variables[VAR_MOMENTUM()+2, i] in
            let momentum_i   = {momentum_i_x, momentum_i_y, momentum_i_z} in
            let density_energy_i = variables[VAR_DENSITY_ENERGY(), i]     in
            let velocity_i   = compute_velocity(density_i, momentum_i)    in
            let speed_sqd_i  = compute_speed_sqd(velocity_i)              in
            let speed_i      = sqrt(speed_sqd_i) in
            let pressure_i   = compute_pressure(density_i, density_energy_i, speed_sqd_i) in
            let speed_of_sound_i = compute_speed_of_sound(density_i, pressure_i) in
            let { flux_contribution_i_momentum_x, flux_contribution_i_momentum_y, 
                  flux_contribution_i_momentum_z,  flux_contribution_i_density_energy } = 
                compute_flux_contribution(density_i, momentum_i, density_energy_i, pressure_i, velocity_i)
            in
            let { flux_contribution_i_momentum_x_x, flux_contribution_i_momentum_x_y, 
                  flux_contribution_i_momentum_x_z } = flux_contribution_i_momentum_x in
            let { flux_contribution_i_momentum_y_x, flux_contribution_i_momentum_y_y, 
                  flux_contribution_i_momentum_y_z } = flux_contribution_i_momentum_y in
            let { flux_contribution_i_momentum_z_x, flux_contribution_i_momentum_z_y, 
                  flux_contribution_i_momentum_z_z } = flux_contribution_i_momentum_z in
            let { flux_contribution_i_density_energy_x, flux_contribution_i_density_energy_y,
                  flux_contribution_i_density_energy_z } = flux_contribution_i_density_energy in
            
            let flux_i_density = 0.0 in
            let flux_i_momentum= {0.0, 0.0, 0.0} in
            let {flux_i_momentum_x, flux_i_momentum_y, flux_i_momentum_z} = flux_i_momentum in
            let flux_i_density_energy = 0.0 in
            let loop_res = {flux_i_density, flux_i_density_energy, flux_i_momentum_x, flux_i_momentum_y, flux_i_momentum_z} in
            loop(loop_res) =
              for j < NNB do
                let {flux_i_density, flux_i_density_energy, flux_i_momentum_x, flux_i_momentum_y, flux_i_momentum_z} = loop_res in
                let nb = elements_surrounding_elements[j, i] in
                let normal_x = normals[0, j, i] in
                let normal_y = normals[1, j, i] in
                let normal_z = normals[2, j, i] in
                let normal_len = sqrt(normal_x*normal_x + normal_y*normal_y + normal_z*normal_z) in
                if (0 <= nb) -- a legitimate neighbor
                then let density_nb    = unsafe variables[VAR_DENSITY(),    nb] in
		     let momentum_nb_x = unsafe variables[VAR_MOMENTUM()+0, nb] in
                     let momentum_nb_y = unsafe variables[VAR_MOMENTUM()+1, nb] in
                     let momentum_nb_z = unsafe variables[VAR_MOMENTUM()+2, nb] in
                     let momentum_nb   = {momentum_nb_x, momentum_nb_y, momentum_nb_z} in
                     let density_energy_nb = unsafe variables[VAR_DENSITY_ENERGY(), nb] in
                     let velocity_nb = compute_velocity(density_nb, momentum_nb) in
                     let speed_sqd_nb= compute_speed_sqd(velocity_nb) in
                     let pressure_nb = compute_pressure(density_nb, density_energy_nb, speed_sqd_nb) in
                     let speed_of_sound_nb = compute_speed_of_sound(density_nb, pressure_nb) in
                     let { flux_contribution_nb_momentum_x, flux_contribution_nb_momentum_y 
                         , flux_contribution_nb_momentum_z, flux_contribution_nb_density_energy } =
                         compute_flux_contribution( density_nb, momentum_nb, density_energy_nb, pressure_nb, velocity_nb ) in

                     let { flux_contribution_nb_density_energy_x, flux_contribution_nb_density_energy_y, 
                           flux_contribution_nb_density_energy_z } = flux_contribution_nb_density_energy in
                     let { flux_contribution_nb_momentum_x_x, flux_contribution_nb_momentum_x_y,
                           flux_contribution_nb_momentum_x_z } = flux_contribution_nb_momentum_x in
                     let { flux_contribution_nb_momentum_y_x, flux_contribution_nb_momentum_y_y,
                           flux_contribution_nb_momentum_y_z } = flux_contribution_nb_momentum_y in
                     let { flux_contribution_nb_momentum_z_x, flux_contribution_nb_momentum_z_y,
                           flux_contribution_nb_momentum_z_z } = flux_contribution_nb_momentum_z in

                     -- artificial viscosity
                     let factor = -normal_len*smoothing_coefficient*0.5*
                                    ( speed_i + sqrt(speed_sqd_nb) + speed_of_sound_i + speed_of_sound_nb ) in
                     let flux_i_density = flux_i_density + factor*(density_i-density_nb) in
                     let flux_i_density_energy = flux_i_density_energy + factor*(density_energy_i-density_energy_nb) in
                     let flux_i_momentum_x = flux_i_momentum_x + factor*(momentum_i_x-momentum_nb_x) in
                     let flux_i_momentum_y = flux_i_momentum_y + factor*(momentum_i_y-momentum_nb_y) in
                     let flux_i_momentum_z = flux_i_momentum_z + factor*(momentum_i_z-momentum_nb_z) in

                     -- accumulate cell-centered fluxes
                     let factor = 0.5*normal_x in
                     let flux_i_density = flux_i_density + 
                             factor*(momentum_nb_x+momentum_i_x) in
                     let flux_i_density_energy = flux_i_density_energy + 
                             factor*(flux_contribution_nb_density_energy_x+flux_contribution_i_density_energy_x) in
                     let flux_i_momentum_x = flux_i_momentum_x + 
                             factor*(flux_contribution_nb_momentum_x_x+flux_contribution_i_momentum_x_x) in
                     let flux_i_momentum_y = flux_i_momentum_y + 
                             factor*(flux_contribution_nb_momentum_y_x+flux_contribution_i_momentum_y_x) in
                     let flux_i_momentum_z = flux_i_momentum_z + 
                             factor*(flux_contribution_nb_momentum_z_x+flux_contribution_i_momentum_z_x) in

                     let factor = 0.5 * normal_y in
                     let flux_i_density = flux_i_density + factor*(momentum_nb_y + momentum_i_y) in
                     let flux_i_density_energy = flux_i_density_energy + 
                             factor*(flux_contribution_nb_density_energy_y+flux_contribution_i_density_energy_y) in
                     let flux_i_momentum_x = flux_i_momentum_x + 
                             factor*(flux_contribution_nb_momentum_x_y+flux_contribution_i_momentum_x_y) in
                     let flux_i_momentum_y = flux_i_momentum_y + 
                             factor*(flux_contribution_nb_momentum_y_y+flux_contribution_i_momentum_y_y) in
                     let flux_i_momentum_z = flux_i_momentum_z + 
                             factor*(flux_contribution_nb_momentum_z_y+flux_contribution_i_momentum_z_y) in

                     let factor = 0.5 * normal_z in
                     let flux_i_density = flux_i_density + factor*(momentum_nb_z+momentum_i_z) in
                     let flux_i_density_energy = flux_i_density_energy + 
                             factor*(flux_contribution_nb_density_energy_z+flux_contribution_i_density_energy_z) in
                     let flux_i_momentum_x = flux_i_momentum_x +
                             factor*(flux_contribution_nb_momentum_x_z+flux_contribution_i_momentum_x_z) in
                     let flux_i_momentum_y = flux_i_momentum_y +
                             factor*(flux_contribution_nb_momentum_y_z+flux_contribution_i_momentum_y_z) in
                     let flux_i_momentum_z = flux_i_momentum_z + 
                             factor*(flux_contribution_nb_momentum_z_z+flux_contribution_i_momentum_z_z) in
                     {flux_i_density, flux_i_density_energy, flux_i_momentum_x, flux_i_momentum_y, flux_i_momentum_z}

                else if(nb == -2)
                then let factor = 0.5*normal_x in
                     let flux_i_density = flux_i_density + factor*(ff_variable[VAR_MOMENTUM()+0]+momentum_i_x) in
                     let flux_i_density_energy = flux_i_density_energy + 
                             factor*(ff_flux_contribution_density_energy_x+flux_contribution_i_density_energy_x) in
                     let flux_i_momentum_x = flux_i_momentum_x + 
                             factor*(ff_flux_contribution_momentum_x_x + flux_contribution_i_momentum_x_x) in
                     let flux_i_momentum_y = flux_i_momentum_y + 
                             factor*(ff_flux_contribution_momentum_y_x + flux_contribution_i_momentum_y_x) in
                     let flux_i_momentum_z = flux_i_momentum_z + 
                             factor*(ff_flux_contribution_momentum_z_x + flux_contribution_i_momentum_z_x) in
                     
                     let factor = 0.5*normal_y in
                     let flux_i_density = flux_i_density + factor*(ff_variable[VAR_MOMENTUM()+1]+momentum_i_y) in
                     let flux_i_density_energy = flux_i_density_energy + 
                             factor*(ff_flux_contribution_density_energy_y+flux_contribution_i_density_energy_y) in
                     let flux_i_momentum_x = flux_i_momentum_x + 
                             factor*(ff_flux_contribution_momentum_x_y + flux_contribution_i_momentum_x_y) in
                     let flux_i_momentum_y = flux_i_momentum_y + 
                             factor*(ff_flux_contribution_momentum_y_y + flux_contribution_i_momentum_y_y) in
                     let flux_i_momentum_z = flux_i_momentum_z + 
                             factor*(ff_flux_contribution_momentum_z_y + flux_contribution_i_momentum_z_y) in

                     let factor = 0.5*normal_z in
                     let flux_i_density = flux_i_density + factor*(ff_variable[VAR_MOMENTUM()+2]+momentum_i_z) in
                     let flux_i_density_energy = flux_i_density_energy + 
                             factor*(ff_flux_contribution_density_energy_z+flux_contribution_i_density_energy_z) in
                     let flux_i_momentum_x = flux_i_momentum_x +
                             factor*(ff_flux_contribution_momentum_x_z + flux_contribution_i_momentum_x_z) in
                     let flux_i_momentum_y = flux_i_momentum_y + 
                             factor*(ff_flux_contribution_momentum_y_z + flux_contribution_i_momentum_y_z) in
                     let flux_i_momentum_z = flux_i_momentum_z + 
                             factor*(ff_flux_contribution_momentum_z_z + flux_contribution_i_momentum_z_z) in
                     {flux_i_density, flux_i_density_energy, flux_i_momentum_x, flux_i_momentum_y, flux_i_momentum_z}

                else if (nb == -1)
                then let flux_i_momentum_x = flux_i_momentum_x + normal_x*pressure_i in
                     let flux_i_momentum_y = flux_i_momentum_y + normal_y*pressure_i in
                     let flux_i_momentum_z = flux_i_momentum_z + normal_z*pressure_i in
                     {flux_i_density, flux_i_density_energy, flux_i_momentum_x, flux_i_momentum_y, flux_i_momentum_z}
                else -- not reachable
                     {flux_i_density, flux_i_density_energy, flux_i_momentum_x, flux_i_momentum_y, flux_i_momentum_z}
            in
            let {flux_i_density, flux_i_density_energy, flux_i_momentum_x, flux_i_momentum_y, flux_i_momentum_z} = loop_res
            in  [flux_i_density, flux_i_momentum_x, flux_i_momentum_y, flux_i_momentum_z, flux_i_density_energy]
            --fluxes[i + VAR_DENSITY*nelr] = flux_i_density;
            --fluxes[i + (VAR_MOMENTUM+0)*nelr] = flux_i_momentum.x;
            --fluxes[i + (VAR_MOMENTUM+1)*nelr] = flux_i_momentum.y;
            --fluxes[i + (VAR_MOMENTUM+2)*nelr] = flux_i_momentum.z;
            --fluxes[i + VAR_DENSITY_ENERGY*nelr] = flux_i_density_energy;

         , iota(nel))
    )

--
fun [[f32,nel],5] time_step( int j, 
                              [[f32,nel],5] old_variables, 
                              [f32,nel] step_factors, 
                              [[f32,nel],5] fluxes  ) =
  transpose(
    map(fn [f32,5] (int i) =>
            let factor = step_factors[i] / f32(RK()+1-j) in
            [ old_variables[VAR_DENSITY(),    i] + factor*fluxes[VAR_DENSITY(),    i]
            , old_variables[VAR_MOMENTUM()+0, i] + factor*fluxes[VAR_MOMENTUM()+0, i]
            , old_variables[VAR_MOMENTUM()+1, i] + factor*fluxes[VAR_MOMENTUM()+1, i]
            , old_variables[VAR_MOMENTUM()+2, i] + factor*fluxes[VAR_MOMENTUM()+2, i]
            , old_variables[VAR_DENSITY_ENERGY(), i] + factor*fluxes[VAR_DENSITY_ENERGY(), i]
            ]
       , iota(nel))
  )

--------------------------
---- MAIN ENTRY POINT ----
--------------------------
fun [[f32,nel],5] 
main(  [f32,nel]       areas, 
      [[int,nel],4]     elements_surrounding_elements, 
     [[[f32,nel],4],3] normals ) =
    let NDIM = 3 in
    let NNB  = 4 in
    let angle_of_attack = (3.1415926535897931 / 180.0) * deg_angle_of_attack() in
    let ff_variable     = copy( replicate(NVAR(), 0.0) ) in
    let ff_variable[VAR_DENSITY()] = 1.4 in
    let ff_pressure = 1.0 in
    let ff_speed_of_sound = sqrt( GAMMA()*ff_pressure / ff_variable[VAR_DENSITY()] ) in
    let ff_speed = ff_mach() * ff_speed_of_sound in
    let { ff_velocity_x, ff_velocity_y, ff_velocity_z } = 
            { ff_speed * 1.0   -- .x   ... cos(angle_of_attack()) = 1
            , ff_speed * 0.0   -- .y   ... sin(angle_of_attack()) = 0
            , 0.0 } in         -- .z
    let ff_velocity = { ff_velocity_x, ff_velocity_y, ff_velocity_z } in
    let ff_variable[VAR_MOMENTUM()+0] = ff_variable[VAR_DENSITY()] * ff_velocity_x in
    let ff_variable[VAR_MOMENTUM()+1] = ff_variable[VAR_DENSITY()] * ff_velocity_y in
    let ff_variable[VAR_MOMENTUM()+2] = ff_variable[VAR_DENSITY()] * ff_velocity_z in
    let ff_variable[VAR_DENSITY_ENERGY()] = ff_variable[VAR_DENSITY()] * (0.5*(ff_speed*ff_speed)) + 
                                            (ff_pressure / (GAMMA()-1.0)) in
    let ff_momentum = 
            { ff_variable[VAR_MOMENTUM()+0]
            , ff_variable[VAR_MOMENTUM()+1]
            , ff_variable[VAR_MOMENTUM()+2]
            } 
    in
    let {   ff_flux_contribution_momentum_x, ff_flux_contribution_momentum_y, 
            ff_flux_contribution_momentum_z, ff_flux_contribution_density_energy    } =
        compute_flux_contribution(  ff_variable[VAR_DENSITY()],        ff_momentum, 
                                    ff_variable[VAR_DENSITY_ENERGY()], ff_pressure, ff_velocity    ) 
    in
    let variables = initialize_variables(nel, ff_variable) 
    in

----  BEGIN DEBUG COSMIN
--    let step_factors = compute_step_factor(variables, areas) in
--    let new_variables= variables in
--    let fluxes = compute_flux(  elements_surrounding_elements, 
--                                normals, new_variables, ff_variable, 
--                                ff_flux_contribution_momentum_x, 
--                                ff_flux_contribution_momentum_y, 
--                                ff_flux_contribution_momentum_z, 
--                                ff_flux_contribution_density_energy )
--    in  time_step(0, variables, step_factors, fluxes)
---- END   DEBUG COSMIN

    loop (variables) =
      for i < iterations() do
        let step_factors = compute_step_factor(variables, areas) in
        let new_variables= variables in
        loop(new_variables) =
          for j < RK() do
            let fluxes = compute_flux(  elements_surrounding_elements, 
                                        normals, new_variables, ff_variable, 
                                        ff_flux_contribution_momentum_x, 
                                        ff_flux_contribution_momentum_y, 
                                        ff_flux_contribution_momentum_z, 
                                        ff_flux_contribution_density_energy )
            in  time_step(j, variables, step_factors, fluxes)
        in  new_variables
    in variables


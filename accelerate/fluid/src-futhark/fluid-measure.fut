--!import fluid-lib

fun {[[real, g], g],
     [[real, g], g],
     [[real, g], g]}
  main([[real, g], g] U0,
       [[real, g], g] V0,
       [[real, g], g] D0,
       int n_steps,
       real time_step,
       real diffusion_rate,
       real viscosity) =
  get_end_frame(U0, V0, D0, n_steps, time_step, diffusion_rate, viscosity)

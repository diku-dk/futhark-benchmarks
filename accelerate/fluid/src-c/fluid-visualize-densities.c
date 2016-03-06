#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include "solver.h"

uint32_t clamp(float x) {
  if (x < 0.0) {
    return 0;
  }
  else if (x > 255.0) {
    return 255;
  }
  else {
    return (uint32_t) x;
  }
}

void draw_densities(size_t N, float* D) {
  for (size_t i = 1; i < N + 1; i++) {
    for (size_t j = 1; j < N + 1; j++) {
      printf("%d ", clamp(255.0 * D[IX(i, j)]));
    }
  }
  printf("%s", "\n");
}

void draw_all_frames(size_t N, float* U, float* V, float* D,
                     uint32_t n_steps, uint32_t n_solver_steps, float time_step,
                     float diffusion_rate, float viscosity) {
  float* tmp0 = (float*) malloc(sizeof(float) * size(N));
  float* tmp1 = (float*) malloc(sizeof(float) * size(N));

  printf("%zd\n", N);
  draw_densities(N, D);
  for (size_t i = 1; i < n_steps; i++) {
    vel_step_light(N, U, V, tmp0, tmp1, viscosity, time_step, n_solver_steps);
    dens_step_light(N, D, tmp0, U, V, diffusion_rate, time_step, n_solver_steps);
    draw_densities(N, D);
  }
}

int main() {
  size_t N;
  scanf("%zu", &N);
  size_t s = size(N);

  float* U0 = (float*) malloc(sizeof(float) * s);
  float* V0 = (float*) malloc(sizeof(float) * s);
  float* D0 = (float*) malloc(sizeof(float) * s);
  int32_t n_steps;
  int32_t n_solver_steps;
  float time_step;
  float diffusion_rate;
  float viscosity;

  
  for (size_t i = 0; i < N + 2; i++) {
    for (size_t j = 0; j < N + 2; j++) {
      scanf("%f", &U0[IX(i, j)]);
    }
  }
  for (size_t i = 0; i < N + 2; i++) {
    for (size_t j = 0; j < N + 2; j++) {
      scanf("%f", &V0[IX(i, j)]);
    }
  }
  for (size_t i = 0; i < N + 2; i++) {
    for (size_t j = 0; j < N + 2; j++) {
      scanf("%f", &D0[IX(i, j)]);
    }
  }
  scanf("%d", &n_steps);
  scanf("%d", &n_solver_steps);
  scanf("%f", &time_step);
  scanf("%f", &diffusion_rate);
  scanf("%f", &viscosity);

  draw_all_frames(N, U0, V0, D0, n_steps, n_solver_steps,
                  time_step, diffusion_rate, viscosity);
  
  return EXIT_SUCCESS;
}

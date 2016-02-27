#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include "solver.h"
#include "timing.h"

void get_end_frame(size_t N, float* U, float* V, float* D,
                   uint32_t n_steps, float time_step,
                   float diffusion_rate, float viscosity) {
  float* tmp0 = (float*) malloc(sizeof(float) * size(N));
  float* tmp1 = (float*) malloc(sizeof(float) * size(N));
  timing_t t;

  timing_start(&t);
  for (size_t i = 1; i < n_steps; i++) {
    vel_step_light(N, U, V, tmp0, tmp1, viscosity, time_step);
    dens_step_light(N, D, tmp0, U, V, diffusion_rate, time_step);
  }
  timing_end(&t);
  fprintf(stderr, "Elapsed milliseconds: %zd\n", t.usecs / 1000);

  printf("%zd\n", N);
  for (size_t i = 0; i < size(N); i++) {
    printf("%f ", U[i]);
  }
  printf("%s", "\n");
  for (size_t i = 0; i < size(N); i++) {
    printf("%f ", V[i]);
  }
  printf("%s", "\n");
  for (size_t i = 0; i < size(N); i++) {
    printf("%f ", D[i]);
  }
  printf("%s", "\n");

}


int main() {
  size_t N;
  scanf("%zu", &N);
  size_t s = size(N);

  float* U0 = (float*) malloc(sizeof(float) * s);
  float* V0 = (float*) malloc(sizeof(float) * s);
  float* D0 = (float*) malloc(sizeof(float) * s);
  int32_t n_steps;
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
  scanf("%f", &time_step);
  scanf("%f", &diffusion_rate);
  scanf("%f", &viscosity);

  get_end_frame(N, U0, V0, D0, n_steps, time_step, diffusion_rate, viscosity);
  
  return EXIT_SUCCESS;
}

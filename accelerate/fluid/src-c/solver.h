#ifndef SOLVER_H
#define SOLVER_H

#include <stdint.h>

#define IX(i, j) ((i) + (N + 2) * (j))

size_t size(size_t N);

/*
 * Memory-light versions.  These versions use the original scheme of
 * aggressively reusing memory.  However, the data flow has been made clearer,
 * since the original code was pretty difficult to comprehend.  The memory usage
 * should be the same.
 */

void dens_step_light(size_t N, float* d, float* tmp, float* u, float* v,
                     float diff, float dt, int n_solver_steps);

void vel_step_light(size_t N, float* u, float* v, float* tmp0, float* tmp1,
                    float visc, float dt, int n_solver_steps);


/*
 * Naive versions.  These versions calculate the same as the memory-light
 * versions, but allocate all temporary storage instead of reusing existing
 * memory.  This was written to make the data flow very clear.
 */

void dens_step_naive(size_t N, float* d_dest, float* d, float* u, float* v,
                     float diff, float dt, int n_solver_steps);

void vel_step_naive(size_t N, float* u_dest, float* v_dest, float* u, float* v,
                    float visc, float dt, int n_solver_steps);


#endif

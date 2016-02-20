/*
 * solver.c --- fluid simulation solver
 *
 * Original author: Jos Stam (jstam@aw.sgi.com)
 * Creation date: Jan 9 2003
 *
 * Modified in 2016 by Niels G. W. Serup (ngws@metanohi.name)
 *
 * This code is a fluid simulation solver based on the code provided in Jos'
 * GDC2003 paper "Real-Time Fluid Dynamics for Games".
 */

#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "solver.h"

#define FOR_EACH_CELL for (size_t i = 1; i <= N; i++) { for (size_t j = 1; j <= N; j++) {
#define END_FOR }}

size_t size(size_t N) {
  return (N + 2) * (N + 2);
}

void set_bnd(size_t N, int b, float* x) {
  for (size_t i = 1; i <= N; i++) {
    x[IX(0  ,i)] = b==1 ? -x[IX(1,i)] : x[IX(1,i)];
    x[IX(N+1,i)] = b==1 ? -x[IX(N,i)] : x[IX(N,i)];
    x[IX(i,0  )] = b==2 ? -x[IX(i,1)] : x[IX(i,1)];
    x[IX(i,N+1)] = b==2 ? -x[IX(i,N)] : x[IX(i,N)];
  }
  x[IX(0  ,0  )] = 0.5f*(x[IX(1,0  )]+x[IX(0  ,1)]);
  x[IX(0  ,N+1)] = 0.5f*(x[IX(1,N+1)]+x[IX(0  ,N)]);
  x[IX(N+1,0  )] = 0.5f*(x[IX(N,0  )]+x[IX(N+1,1)]);
  x[IX(N+1,N+1)] = 0.5f*(x[IX(N,N+1)]+x[IX(N+1,N)]);
}

void lin_solve(size_t N, int b, float* x, float* x_prev, float a, float c) {
  /* BEGIN FIX.  In the original C version, the x array in the first iteration
     contains the interactively added sources, which doesn't make really sense.
     This happens because a lot of arrays are reused, and probably because it
     doesn't change much, as the source array is surely almost zero in all
     cells. */
  for (size_t i = 0; i < size(N); i++) {
    x[i] = 0;
  }
  /* END FIX. */
    
  for (int k = 0; k < 20; k++) {
    FOR_EACH_CELL {
      x[IX(i, j)] = ((x_prev[IX(i, j)]
                      + a * (x[IX(i - 1, j)]
                             + x[IX(i + 1, j)]
                             + x[IX(i, j - 1)]
                             + x[IX(i ,j + 1)])) / c);
    } END_FOR;
    set_bnd(N, b, x);
  }
}

void diffuse(size_t N, int b, float* x, float* x_prev, float diff, float dt) {
  float a=dt*diff*N*N;
  lin_solve(N, b, x, x_prev, a, 1+4*a);
}

void advect(size_t N, int b, float* d, float* d0, float* u, float* v, float dt) {
  int i0, j0, i1, j1;
  float x, y, s0, t0, s1, t1, dt0;
  
  dt0 = dt * N;
  FOR_EACH_CELL {
    x = i-dt0*u[IX(i,j)]; y = j-dt0*v[IX(i,j)];

    if (x<0.5f) {
      x=0.5f;
    }
    if (x>N+0.5f) {
      x=N+0.5f;
    }
    i0=(int)x;
    i1=i0+1;
    
    if (y<0.5f) {
      y=0.5f;
    }
    if (y>N+0.5f) {
      y=N+0.5f;
    }
    j0=(int)y;
    j1=j0+1;

    s1 = x-i0;
    s0 = 1-s1;
    t1 = y-j0;
    t0 = 1-t1;

    d[IX(i,j)] = (s0*(t0*d0[IX(i0,j0)]+t1*d0[IX(i0,j1)])
                  + s1*(t0*d0[IX(i1,j0)]+t1*d0[IX(i1,j1)]));
  } END_FOR;
  set_bnd (N, b, d);
}

void project(size_t N, float* u, float* v, float* u0, float* v0,
             float* tmp0, float* tmp1) {
  float* div = tmp0;
  float* p = tmp1;
  
  FOR_EACH_CELL {
    div[IX(i,j)] = -0.5f*(u0[IX(i+1,j)]-u0[IX(i-1,j)]+v0[IX(i,j+1)]-v0[IX(i,j-1)])/N;
  } END_FOR;
  set_bnd(N, 0, div);

  lin_solve(N, 0, p, div, 1, 4);

  FOR_EACH_CELL {
    u[IX(i,j)] = u0[IX(i,j)] - 0.5f*N*(p[IX(i+1,j)]-p[IX(i-1,j)]);
    v[IX(i,j)] = v0[IX(i,j)] - 0.5f*N*(p[IX(i,j+1)]-p[IX(i,j-1)]);
  } END_FOR;
  set_bnd(N, 1, u);
  set_bnd(N, 2, v);
}

void dens_step_naive(size_t N, float* d_dest, float* d, float* u, float* v,
                     float diff, float dt) {
  int s = size(N);
  float* d0 = (float*) malloc(sizeof(float) * s);
  float* d1 = (float*) malloc(sizeof(float) * s);
  float* d2 = (float*) malloc(sizeof(float) * s);
  
  memcpy(d0, d, sizeof(float) * s);
  
  diffuse(N, 0, d1, d0, diff, dt);
  advect(N, 0, d2, d1, u, v, dt);

  memcpy(d_dest, d2, sizeof(float) * s);

  free(d0);
  free(d1);
  free(d2);
}

void vel_step_naive(size_t N, float* u_dest, float* v_dest, float* u, float* v,
                    float visc, float dt) {
  int s = size(N);
  float* u0 = (float*) malloc(sizeof(float) * s);
  float* v0 = (float*) malloc(sizeof(float) * s);
  float* u1 = (float*) malloc(sizeof(float) * s);
  float* v1 = (float*) malloc(sizeof(float) * s);
  float* u2 = (float*) malloc(sizeof(float) * s);
  float* v2 = (float*) malloc(sizeof(float) * s);
  float* u3 = (float*) malloc(sizeof(float) * s);
  float* v3 = (float*) malloc(sizeof(float) * s);
  float* u4 = (float*) malloc(sizeof(float) * s);
  float* v4 = (float*) malloc(sizeof(float) * s);
  float* tmp0 = (float*) malloc(sizeof(float) * s);
  float* tmp1 = (float*) malloc(sizeof(float) * s);

  memcpy(u0, u, sizeof(float) * s);
  memcpy(v0, v, sizeof(float) * s);

  diffuse(N, 1, u1, u0, visc, dt);
  diffuse(N, 2, v1, v0, visc, dt);

  project(N, u2, v2, u1, v1, tmp0, tmp1);

  advect(N, 1, u3, u2, u2, v2, dt);
  advect(N, 2, v3, v2, u2, v2, dt);

  project(N, u4, v4, u3, v3, tmp0, tmp1);

  memcpy(u_dest, u4, sizeof(float) * s);
  memcpy(v_dest, v4, sizeof(float) * s);

  free(u0);
  free(v0);
  free(u1);
  free(v1);
  free(u2);
  free(v2);
  free(u3);
  free(v3);
  free(u4);
  free(v4);
  free(tmp0);
  free(tmp1);
}

void dens_step_light(size_t N, float* d, float* tmp, float* u, float* v,
                     float diff, float dt) {
  float* d0;
  float* d1;
  float* d2;

  d0 = d;

  d1 = tmp;
  diffuse(N, 0, d1, d0, diff, dt);
  d2 = d;
  advect(N, 0, d2, d1, u, v, dt);
}

void vel_step_light(size_t N, float* u, float* v, float* tmp0, float* tmp1,
                    float visc, float dt) {
  float* u0;
  float* v0;
  float* u1;
  float* v1;
  float* u2;
  float* v2;
  float* u3;
  float* v3;
  float* u4;
  float* v4;
  float* tmp0_cur;
  float* tmp1_cur;

  u0 = u;
  v0 = v;

  u1 = tmp0;
  v1 = tmp1;
  diffuse(N, 1, u1, u0, visc, dt);
  diffuse(N, 2, v1, v0, visc, dt);

  u2 = tmp0;
  v2 = tmp1;
  tmp0_cur = u;
  tmp1_cur = v;
  project(N, u2, v2, u1, v1, tmp0_cur, tmp1_cur);

  u3 = u;
  v3 = v;
  advect(N, 1, u3, u2, u2, v2, dt);
  advect(N, 2, v3, v2, u2, v2, dt);

  u4 = u;
  v4 = v;
  project(N, u4, v4, u3, v3, tmp0, tmp1);
}

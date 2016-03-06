/*
 * demo.c --- protoype to show off the fluid simulation solver
 *
 * Original author: Jos Stam (jstam@aw.sgi.com)
 * Creation date: Jan 9 2003
 *
 * Modified in 2016 by Niels G. W. Serup (ngws@metanohi.name)
 *
 * This code is a simple prototype that demonstrates how to use the code
 * provided in Jos' GDC2003 paper "Real-Time Fluid Dynamics for Games".  This
 * code uses OpenGL and GLUT for graphics and interface.
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <GL/freeglut.h>
#include "solver.h"


/* Global variables. */

static size_t N;
static float dt, diff, visc;
static float force, source;
static int dvel;

static float* u;
static float* v;
static float* u_prev;
static float* v_prev;
static float* dens;
static float* dens_prev;

static int win_id;
static int win_x, win_y;
static int mouse_down[3];
static int omx, omy, mx, my;


/*
 * Free/clear/allocate simulation data.
 */

static void free_data(void) {
  if (u != NULL) free(u);
  if (v != NULL) free(v);
  if (u_prev != NULL) free(u_prev);
  if (v_prev != NULL) free(v_prev);
  if (dens != NULL) free(dens);
  if (dens_prev != NULL) free(dens_prev);
}

static void clear_data(void) {
  for (size_t i = 0; i < size(N); i++) {
    u[i] = v[i] = u_prev[i] = v_prev[i] = dens[i] = dens_prev[i] = 0.0f;
  }
}

static bool allocate_data(void) {
  int s = size(N);
  u = (float*) malloc(s * sizeof(float));
  v = (float*) malloc(s * sizeof(float));
  u_prev = (float*) malloc(s * sizeof(float));
  v_prev = (float*) malloc(s * sizeof(float));
  dens = (float*) malloc(s * sizeof(float));
  dens_prev	= (float*) malloc(s * sizeof(float));

  if (!u || !v || !u_prev || !v_prev || !dens || !dens_prev) {
    fprintf(stderr, "error: cannot allocate data\n");
    return false;
  }

  return true;
}


/*
 * OpenGL-specific drawing routines.
 */

static void pre_display(void) {
  glViewport(0, 0, win_x, win_y);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluOrtho2D(0.0, 1.0, 0.0, 1.0);
  glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
  glClear(GL_COLOR_BUFFER_BIT);
}

static void post_display(void) {
  glutSwapBuffers();
}

static void draw_velocity(void) {
  float x, y, h;

  h = 1.0f / N;

  glColor3f(1.0f, 1.0f, 1.0f);
  glLineWidth(1.0f);

  glBegin(GL_LINES);

  for (size_t i = 1; i <= N; i++) {
    x = (i - 0.5f) * h;
    for (size_t j = 1; j <= N; j++) {
      y = (j - 0.5f) * h;
      
      glVertex2f(x, y);
      glVertex2f(x + u[IX(i,j)], y + v[IX(i,j)]);
    }
  }
  
  glEnd();
}

static void draw_density(void) {
  float x, y, h, d00, d01, d10, d11;

  h = 1.0f / N;

  glBegin(GL_QUADS);

  for (size_t i = 0; i <= N; i++) {
    x = (i - 0.5f) * h;
    for (size_t j = 0; j <= N; j++) {
      y = (j - 0.5f) * h;

      d00 = dens[IX(i,j)];
      d01 = dens[IX(i,j+1)];
      d10 = dens[IX(i+1,j)];
      d11 = dens[IX(i+1,j+1)];

      glColor3f(d00, d00, d00); glVertex2f(x, y);
      glColor3f(d10, d10, d10); glVertex2f(x + h, y);
      glColor3f(d11, d11, d11); glVertex2f(x + h, y + h);
      glColor3f(d01, d01, d01); glVertex2f(x, y + h);
    }
  }

  glEnd();
}


/*
 * Relates mouse movements to forces sources.
 */

static void get_from_UI(float* d, float* u, float* v) {
  size_t i, j;

  for (i = 0; i < size(N); i++) {
    u[i] = v[i] = d[i] = 0.0f;
  }

  if (!mouse_down[0] && !mouse_down[2]) {
    return;
  }

  i = (int) ((mx / (float) win_x) * N + 1);
  j = (int) (((win_y - my) / (float) win_y) * N + 1);

  if (i < 1 || i > N || j < 1 || j > N) {
    return;
  }
  
  if (mouse_down[0]) {
    u[IX(i,j)] = force * (mx - omx);
    v[IX(i,j)] = force * (omy - my);
  }

  if (mouse_down[2]) {
    d[IX(i,j)] = source;
  }

  omx = mx;
  omy = my;
}

/*
 * GLUT callback routines.
 */

static void key_func(unsigned char key,
                     __attribute__((unused)) int x,
                     __attribute__((unused)) int y) {
  switch (key) {
  case 'c':
  case 'C':
    clear_data();
    break;

  case 'q':
  case 'Q':
    free_data();
    glutLeaveMainLoop();
    exit(EXIT_SUCCESS);
    break;

  case 'v':
  case 'V':
    dvel = !dvel;
    break;
  }
}

static void mouse_func(int button, int state, int x, int y) {
  omx = mx = x;
  omx = my = y;

  mouse_down[button] = state == GLUT_DOWN;
}

static void motion_func(int x, int y) {
  mx = x;
  my = y;
}

static void reshape_func(int width, int height) {
  glutSetWindow(win_id);
  glutReshapeWindow(width, height);

  win_x = width;
  win_y = height;
}

void add_source(float* x, float* s, float dt) {
  for (size_t i = 0; i < size(N); i++) {
    x[i] += dt * s[i];
  }
}

static void idle_func (void) {
  bool memory_naive = false; /* Reuse memory! */
  
  get_from_UI(dens_prev, u_prev, v_prev);

  add_source(u, u_prev, dt);
  add_source(v, v_prev, dt);
  add_source(dens, dens_prev, dt);

  uint32_t n_solver_steps = 20;

  if (memory_naive) {
    vel_step_naive(N, u, v, u, v, visc, dt, n_solver_steps);
  }
  else {
    /* u_prev and v_prev are overwritten on next GUI read anyway, so we use them
       as temporary arrays. */
    vel_step_light(N, u, v, u_prev, v_prev, visc, dt, n_solver_steps);
  }

  if (memory_naive) {
    dens_step_naive(N, dens, dens, u, v, diff, dt, n_solver_steps);
  }
  else {
    /* dens_prev is overwritten on next GUI read anyway, so we use it as a
       temporary array. */
    dens_step_light(N, dens, dens_prev, u, v, diff, dt, n_solver_steps);
  }

  glutSetWindow(win_id);
  glutPostRedisplay();
}

static void display_func(void) {
  pre_display();

  if (dvel) {
    draw_velocity();
  }
  else {
    draw_density();
  }

  post_display();
}


/*
 * open_glut_window --- open a glut compatible window and set callbacks.
 */

static void open_glut_window(void) {
  glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE);

  glutInitWindowPosition(0, 0);
  glutInitWindowSize(win_x, win_y);
  win_id = glutCreateWindow ( "Alias | wavefront" );

  glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
  glClear(GL_COLOR_BUFFER_BIT);
  glutSwapBuffers();
  glClear(GL_COLOR_BUFFER_BIT);
  glutSwapBuffers();

  pre_display();

  glutKeyboardFunc(key_func);
  glutMouseFunc(mouse_func);
  glutMotionFunc(motion_func);
  glutReshapeFunc(reshape_func);
  glutIdleFunc(idle_func);
  glutDisplayFunc(display_func);
}


/*
 * main.
 */

int main(int argc, char* argv[]) {
  glutInit(&argc, argv);

  if (argc != 1 && argc != 6) {
    fprintf(stderr, "usage : %s N dt diff visc force source\n", argv[0]);
    fprintf(stderr, "where:\n");
    fprintf(stderr, "\t N      : grid resolution\n");
    fprintf(stderr, "\t dt     : time step\n");
    fprintf(stderr, "\t diff   : diffusion rate of the density\n");
    fprintf(stderr, "\t visc   : viscosity of the fluid\n");
    fprintf(stderr, "\t force  : scales the mouse movement that generate a force\n");
    fprintf(stderr, "\t source : amount of density that will be deposited\n");
    exit(EXIT_FAILURE);
  }

  if (argc == 1) {
    N = 64;
    dt = 0.1f;
    diff = 0.0f;
    visc = 0.0f;
    force = 5.0f;
    source = 100.0f;
    fprintf(stderr, "Using defaults : N=%zd dt=%g diff=%g visc=%g force = %g source=%g\n",
			N, dt, diff, visc, force, source);
  }
  else {
    N = atoi(argv[1]);
    dt = atof(argv[2]);
    diff = atof(argv[3]);
    visc = atof(argv[4]);
    force = atof(argv[5]);
    source = atof(argv[6]);
  }

  printf("\n\nHow to use this demo:\n\n");
  printf("\t Add densities with the right mouse button\n");
  printf("\t Add velocities with the left mouse button and dragging the mouse\n");
  printf("\t Toggle density/velocity display with the 'v' key\n");
  printf("\t Clear the simulation by pressing the 'c' key\n");
  printf("\t Quit by pressing the 'q' key\n");

  dvel = 0;
  
  if (!allocate_data ()) {
    exit(EXIT_FAILURE);
  }
  clear_data();

  win_x = 512;
  win_y = 512;
  open_glut_window();

  glutMainLoop();

  return EXIT_SUCCESS;
}

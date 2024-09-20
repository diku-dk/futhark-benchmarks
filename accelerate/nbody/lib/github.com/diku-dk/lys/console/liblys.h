#ifndef LIBLYS_HEADER
#define LIBLYS_HEADER

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <termios.h>

#include PROGHEADER

#include "shared.h"

enum lys_event {
  LYS_LOOP_START,
  LYS_LOOP_ITERATION,
  LYS_LOOP_END,
  LYS_WINDOW_SIZE_UPDATED,
  LYS_F1
};

struct lys_context {
  struct futhark_context *fut;
  struct futhark_opaque_state *state;
  int width;
  int height;
  uint32_t *fgs;
  uint32_t *bgs;
  char *chars;
  uint32_t *rgbs;
  int64_t last_time;
  bool running;
  bool grab_mouse;
  bool mouse_grabbed;
  float fps;
  int max_fps;
  int num_frames;
  void* event_handler_data;
  void (*event_handler)(struct lys_context*, enum lys_event);
  int key_pressed;
  bool interactive;
  FILE* out;
};

void lys_setup(struct lys_context *ctx, int max_fps, int num_frames, FILE *output, int width, int height);

void lys_run_console(struct lys_context *ctx);

void draw_text(struct lys_context *ctx, char* buffer, int32_t colour,
               int x_start, int y_start);

#endif

#ifndef LIBLYS_HEADER
#define LIBLYS_HEADER

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>

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
  SDL_Window *wnd;
  SDL_Surface *wnd_surface;
  SDL_Surface *surface;
  int width;
  int height;
  uint32_t *data;
  int64_t last_time;
  bool running;
  bool grab_mouse;
  bool mouse_grabbed;
  float fps;
  int max_fps;
  int sdl_flags;
  void* event_handler_data;
  void (*event_handler)(struct lys_context*, enum lys_event);
  TTF_Font *font;
  int font_size;
};

#define SDL_ASSERT(x) _sdl_assert(x, __FILE__, __LINE__)
static inline void _sdl_assert(int res, const char *file, int line) {
  if (res == 0) {
    fprintf(stderr, "%s:%d: SDL error %d: %s\n",
            file, line, res, SDL_GetError());
    exit(EXIT_FAILURE);
  }
}

void lys_setup(struct lys_context *ctx, int width, int height, int max_fps, int sdl_flags);

void lys_run_sdl(struct lys_context *ctx);

#ifdef LYS_TTF
void draw_text(struct lys_context *ctx, TTF_Font *font, int font_size, char* buffer, int32_t colour,
               int x_start, int y_start);
#endif

#endif

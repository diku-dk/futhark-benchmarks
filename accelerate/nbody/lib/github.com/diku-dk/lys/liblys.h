#ifndef LIBLYS_HEADER
#define LIBLYS_HEADER

#include PROGHEADER
#include <stdio.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>
#include <stdbool.h>

struct lys_context {
  struct futhark_context *fut;
  struct futhark_opaque_state *state;
  SDL_Window *wnd;
  SDL_Surface *wnd_surface;
  SDL_Surface *surface;
  TTF_Font *font;
  char* font_path;
  int font_size;
  int width;
  int height;
  int32_t *data;
  int64_t last_time;
  bool running;
  char* text_format;
  char* text_buffer;
  size_t text_buffer_len;
  bool show_text;
  bool grab_mouse;
  bool mouse_grabbed;
  float fps;
  int max_fps;
  char* **sum_names;
};

#define FUT_CHECK(ctx, x) _fut_check(ctx, x, __FILE__, __LINE__)
static inline void _fut_check(struct futhark_context *ctx, int res,
                              const char *file, int line) {
  if (res != 0) {
    fprintf(stderr, "%s:%d: Futhark error %d: %s\n",
            file, line, res, futhark_context_get_error(ctx));
    exit(EXIT_FAILURE);
  }
}

#endif

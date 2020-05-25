// Convenience framework for writing visualisations with Futhark and
// C/SDL.
//
// Based on initial SDL wrapper code by Jakob Stokholm Bertelsen.

#include "liblys.h"


static void trigger_event(struct lys_context *ctx, enum lys_event event) {
  ctx->event_handler(ctx, event);
}

static void window_size_updated(struct lys_context *ctx, int newx, int newy) {
  // https://stackoverflow.com/a/40122002
  ctx->wnd_surface = SDL_GetWindowSurface(ctx->wnd);
  SDL_ASSERT(ctx->wnd_surface != NULL);

  ctx->width = newx;
  ctx->height = newy;

  struct futhark_opaque_state *new_state;
  FUT_CHECK(ctx->fut, futhark_entry_resize(ctx->fut, &new_state, ctx->height, ctx->width, ctx->state));
  futhark_free_opaque_state(ctx->fut, ctx->state);
  ctx->state = new_state;

  ctx->wnd_surface = SDL_GetWindowSurface(ctx->wnd);
  SDL_ASSERT(ctx->wnd_surface != NULL);

  if (ctx->data != NULL) {
    free(ctx->data);
  }
  ctx->data = malloc(ctx->width * ctx->height * sizeof(uint32_t));
  assert(ctx->data != NULL);

  if (ctx->surface != NULL) {
    SDL_FreeSurface(ctx->surface);
  }
  ctx->surface = SDL_CreateRGBSurfaceFrom(ctx->data, ctx->width, ctx->height,
                                          32, ctx->width * sizeof(uint32_t), 0xFF0000, 0xFF00, 0xFF, 0x00000000);
  SDL_ASSERT(ctx->surface != NULL);

  trigger_event(ctx, LYS_WINDOW_SIZE_UPDATED);
}

static void mouse_event(struct lys_context *ctx, Uint32 state, int x, int y) {
  // We ignore mouse events if we are running a program that would
  // like mouse grab, but where we have temporarily taken the mouse
  // back from it (to e.g. resize the window).
  if (ctx->grab_mouse != ctx->mouse_grabbed) {
    return;
  }

  struct futhark_opaque_state *new_state;
  FUT_CHECK(ctx->fut, futhark_entry_mouse(ctx->fut, &new_state, state, x, y, ctx->state));
  futhark_free_opaque_state(ctx->fut, ctx->state);
  ctx->state = new_state;
}

static void wheel_event(struct lys_context *ctx, int x, int y) {
  struct futhark_opaque_state *new_state;
  FUT_CHECK(ctx->fut, futhark_entry_wheel(ctx->fut, &new_state, x, y, ctx->state));
  futhark_free_opaque_state(ctx->fut, ctx->state);
  ctx->state = new_state;
}

static void handle_sdl_events(struct lys_context *ctx) {
  SDL_Event event;

  while (SDL_PollEvent(&event) == 1) {
    switch (event.type) {
    case SDL_WINDOWEVENT:
      switch (event.window.event) {
      case SDL_WINDOWEVENT_RESIZED:
        {
          int newx = (int)event.window.data1;
          int newy = (int)event.window.data2;
          window_size_updated(ctx, newx, newy);
          break;
        }
      }
      break;
    case SDL_QUIT:
      ctx->running = 0;
      break;
    case SDL_MOUSEMOTION:
      if (ctx->grab_mouse) {
        mouse_event(ctx, event.motion.state, event.motion.xrel, event.motion.yrel);
      } else {
        mouse_event(ctx, event.motion.state, event.motion.x, event.motion.y);
      }
      break;
    case SDL_MOUSEBUTTONDOWN:
    case SDL_MOUSEBUTTONUP:
      if (ctx->grab_mouse && !ctx->mouse_grabbed) {
        assert(SDL_SetRelativeMouseMode(1) == 0);
        ctx->mouse_grabbed = 1;
      }

      if (ctx->grab_mouse) {
        mouse_event(ctx, 1<<(event.button.button-1), event.motion.xrel, event.motion.yrel);
      } else {
        mouse_event(ctx, 1<<(event.button.button-1), event.motion.x, event.motion.y);
      }
      break;
    case SDL_MOUSEWHEEL:
      wheel_event(ctx, event.wheel.x, event.wheel.y);
      break;
    case SDL_KEYDOWN:
    case SDL_KEYUP:
      switch (event.key.keysym.sym) {
      case SDLK_ESCAPE:
        if (ctx->grab_mouse && ctx->mouse_grabbed) {
          assert(SDL_SetRelativeMouseMode(0) == 0);
          ctx->mouse_grabbed = 0;
        } else if (event.key.type == SDL_KEYDOWN) {
          ctx->running = 0;
        }
        break;
      case SDLK_F1:
        if (event.key.type == SDL_KEYDOWN) {
          trigger_event(ctx, LYS_F1);
        }
        break;
      default:
        {
          struct futhark_opaque_state *new_state;
          int e = event.key.type == SDL_KEYDOWN ? 0 : 1;
          FUT_CHECK(ctx->fut, futhark_entry_key(ctx->fut, &new_state,
                                                e, event.key.keysym.sym, ctx->state));
          futhark_free_opaque_state(ctx->fut, ctx->state);
          ctx->state = new_state;
        }
      }
    }
  }
}

static void sdl_loop(struct lys_context *ctx) {
  struct futhark_i32_2d *out_arr;

  while (ctx->running) {
    int64_t now = lys_wall_time();
    float delta = ((float)(now - ctx->last_time))/1000000;
    ctx->fps = (ctx->fps*0.9 + (1/delta)*0.1);
    ctx->last_time = now;
    struct futhark_opaque_state *new_state;
    FUT_CHECK(ctx->fut, futhark_entry_step(ctx->fut, &new_state, delta, ctx->state));
    futhark_free_opaque_state(ctx->fut, ctx->state);
    ctx->state = new_state;

    FUT_CHECK(ctx->fut, futhark_entry_render(ctx->fut, &out_arr, ctx->state));
    FUT_CHECK(ctx->fut, futhark_values_i32_2d(ctx->fut, out_arr, ctx->data));
    FUT_CHECK(ctx->fut, futhark_free_i32_2d(ctx->fut, out_arr));

    SDL_ASSERT(SDL_BlitSurface(ctx->surface, NULL, ctx->wnd_surface, NULL)==0);

    trigger_event(ctx, LYS_LOOP_ITERATION);

    SDL_ASSERT(SDL_UpdateWindowSurface(ctx->wnd) == 0);

    SDL_Delay((int) (1000.0 / ctx->max_fps - delta / 1000));

    handle_sdl_events(ctx);
  }
}

void lys_run_sdl(struct lys_context *ctx) {
  struct futhark_context *fut = ctx->fut;

  ctx->last_time = lys_wall_time();

  ctx->wnd =
    SDL_CreateWindow("Lys",
                     SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
                     ctx->width, ctx->height, ctx->sdl_flags);
  SDL_ASSERT(ctx->wnd != NULL);

  window_size_updated(ctx, ctx->width, ctx->height);

  ctx->running = 1;
  ctx->mouse_grabbed = 0;

  if (ctx->grab_mouse) {
    assert(SDL_SetRelativeMouseMode(1) == 0);
    ctx->mouse_grabbed = 1;
  }

  trigger_event(ctx, LYS_LOOP_START);

  sdl_loop(ctx);

  FUT_CHECK(fut, futhark_free_opaque_state(fut, ctx->state));

  trigger_event(ctx, LYS_LOOP_END);

  SDL_FreeSurface(ctx->surface);
  // do not free wnd_surface (see SDL_GetWindowSurface)
  SDL_DestroyWindow(ctx->wnd);
  SDL_Quit();
}

void lys_setup(struct lys_context *ctx, int width, int height, int max_fps, int sdl_flags) {
  memset(ctx, 0, sizeof(struct lys_context));
  ctx->width = width;
  ctx->height = height;
  ctx->fps = 0;
  ctx->max_fps = max_fps;
  ctx->sdl_flags = sdl_flags;

  SDL_ASSERT(SDL_Init(SDL_INIT_EVERYTHING) == 0);
}

#ifdef LYS_TTF
void draw_text(struct lys_context *ctx,
               TTF_Font *font, int font_size,
               char* buffer, int32_t colour,
               int y_start, int x_start) {
  SDL_Surface *text_surface;
  SDL_Rect offset_rect;

  SDL_Color sdl_colour =
      { .a = (colour >> 24) & 0xff,
        .r = (colour >> 16) & 0xff,
        .g = (colour >> 8) & 0xff,
        .b = colour & 0xff };

  offset_rect.x = x_start;
  int y = y_start;
  while (true) {
    char* buffer_start = buffer;

    bool no_more_text = false;
    while (true) {
      if (*buffer == '\n') {
        *buffer = '\0';
        break;
      } else if (*buffer == '\0') {
        no_more_text = true;
        break;
      }
      buffer++;
    }

    if (*buffer_start != '\0') {
      text_surface = TTF_RenderUTF8_Blended(font, buffer_start, sdl_colour);
      SDL_ASSERT(text_surface != NULL);
      offset_rect.y = y;
      offset_rect.w = text_surface->w;
      offset_rect.h = text_surface->h;
      SDL_ASSERT(SDL_BlitSurface(text_surface, NULL,
                                 ctx->wnd_surface, &offset_rect) == 0);
      SDL_FreeSurface(text_surface);
    }

    if (no_more_text) {
      break;
    } else {
      buffer++;
      y += font_size;
    }
  }
}
#endif

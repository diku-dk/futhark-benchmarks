// Convenience framework for writing visualisations with Futhark and
// C/SDL.

#define CL_SILENCE_DEPRECATION

#define _XOPEN_SOURCE
#include PROGHEADER

#include <inttypes.h>
#include <assert.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>

static int64_t get_wall_time(void) {
  struct timeval time;
  assert(gettimeofday(&time,NULL) == 0);
  return time.tv_sec * 1000000 + time.tv_usec;
}

#define FPS 60
#define INITIAL_WIDTH 250
#define INITIAL_HEIGHT 250

#define SDL_ASSERT(x) _sdl_assert(x, __FILE__, __LINE__)
static inline void _sdl_assert(int res, const char *file, int line) {
  if (res == 0) {
    fprintf(stderr, "%s:%d: SDL error %d: %s\n",
            file, line, res, SDL_GetError());
    exit(EXIT_FAILURE);
  }
}

#define FUT_CHECK(ctx, x) _fut_check(ctx, x, __FILE__, __LINE__)
static inline void _fut_check(struct futhark_context *ctx, int res,
                              const char *file, int line) {
  if (res != 0) {
    fprintf(stderr, "%s:%d: Futhark error %d: %s\n",
            file, line, res, futhark_context_get_error(ctx));
    exit(EXIT_FAILURE);
  }
}

struct lys_context {
  struct futhark_context *fut;
  struct futhark_opaque_state *state;
  SDL_Window *wnd;
  SDL_Surface *wnd_surface;
  SDL_Surface *surface;
  TTF_Font *font;
  int width;
  int height;
  int32_t *data;
  int64_t last_time;
  int running;
};

void window_size_updated(struct lys_context *ctx, int newx, int newy) {
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
}

void mouse_event(struct lys_context *ctx, Uint32 state, int x, int y) {
  struct futhark_opaque_state *new_state;
  FUT_CHECK(ctx->fut, futhark_entry_mouse(ctx->fut, &new_state, state, x, y, ctx->state));
  futhark_free_opaque_state(ctx->fut, ctx->state);
  ctx->state = new_state;
}

void wheel_event(struct lys_context *ctx, int x, int y) {
  struct futhark_opaque_state *new_state;
  FUT_CHECK(ctx->fut, futhark_entry_wheel(ctx->fut, &new_state, x, y, ctx->state));
  futhark_free_opaque_state(ctx->fut, ctx->state);
  ctx->state = new_state;
}

void handle_sdl_events(struct lys_context *ctx) {
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
      mouse_event(ctx, event.motion.state, event.motion.x, event.motion.y);
      break;
    case SDL_MOUSEBUTTONDOWN:
    case SDL_MOUSEBUTTONUP:
      mouse_event(ctx, 1<<(event.button.button-1), event.motion.x, event.motion.y);
      break;
    case SDL_MOUSEWHEEL:
      wheel_event(ctx, event.wheel.x, event.wheel.y);
      break;
    case SDL_KEYDOWN:
    case SDL_KEYUP:
      switch (event.key.keysym.sym) {
      case SDLK_ESCAPE:
        ctx->running = 0;
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

void sdl_loop(struct lys_context *ctx) {
  struct futhark_i32_2d *out_arr;

  while (ctx->running) {
    int64_t now = get_wall_time();
    float delta = ((float)(now - ctx->last_time))/1000000;
    ctx->last_time = now;
    struct futhark_opaque_state *new_state;
    FUT_CHECK(ctx->fut, futhark_entry_step(ctx->fut, &new_state, delta, ctx->state));
    futhark_free_opaque_state(ctx->fut, ctx->state);
    ctx->state = new_state;

    int64_t render_start = get_wall_time();
    FUT_CHECK(ctx->fut, futhark_entry_render(ctx->fut, &out_arr, ctx->state));
    FUT_CHECK(ctx->fut, futhark_context_sync(ctx->fut));
    int64_t render_end = get_wall_time();
    float render_milliseconds = ((float) (render_end - render_start)) / 1000.0;
    FUT_CHECK(ctx->fut, futhark_values_i32_2d(ctx->fut, out_arr, ctx->data));
    FUT_CHECK(ctx->fut, futhark_free_i32_2d(ctx->fut, out_arr));

    SDL_ASSERT(SDL_BlitSurface(ctx->surface, NULL, ctx->wnd_surface, NULL)==0);


    struct futhark_i32_2d *f_ss;
    struct futhark_i32_2d *f_tss;
    struct futhark_f32_2d *f_fss;
    struct futhark_i32_2d *f_iss;
    struct futhark_i32_1d *f_cs;
    FUT_CHECK(ctx->fut, futhark_entry_text(ctx->fut, &f_ss, &f_tss,
                                           &f_fss, &f_iss, &f_cs,
                                           render_milliseconds, ctx->state));
    int64_t *ss_shape = futhark_shape_i32_2d(ctx->fut, f_ss);
    int64_t *tss_shape = futhark_shape_i32_2d(ctx->fut, f_tss);
    int64_t *fss_shape = futhark_shape_f32_2d(ctx->fut, f_fss);
    int64_t *iss_shape = futhark_shape_i32_2d(ctx->fut, f_iss);
    int64_t *cs_shape = futhark_shape_i32_1d(ctx->fut, f_cs);

    int32_t *ss = malloc(sizeof(int32_t) * ss_shape[0] * ss_shape[1]);
    assert(ss != NULL);
    int32_t *tss = malloc(sizeof(int32_t) * tss_shape[0] * tss_shape[1]);
    assert(tss != NULL);
    float *fss = malloc(sizeof(float) * fss_shape[0] * fss_shape[1]);
    assert(fss != NULL);
    int32_t *iss = malloc(sizeof(int32_t) * iss_shape[0] * iss_shape[1]);
    assert(iss != NULL);
    int32_t *cs = malloc(sizeof(int32_t) * cs_shape[0]);
    assert(cs != NULL);

    FUT_CHECK(ctx->fut, futhark_values_i32_2d(ctx->fut, f_ss, ss));
    FUT_CHECK(ctx->fut, futhark_values_i32_2d(ctx->fut, f_tss, tss));
    FUT_CHECK(ctx->fut, futhark_values_f32_2d(ctx->fut, f_fss, fss));
    FUT_CHECK(ctx->fut, futhark_values_i32_2d(ctx->fut, f_iss, iss));
    FUT_CHECK(ctx->fut, futhark_values_i32_1d(ctx->fut, f_cs, cs));

    SDL_Surface *text_surface;
    SDL_Rect offset_rect;
    offset_rect.x = 10;
    SDL_Color text_color;
    text_color.a = 0xff;
    char text[200];
    int y = 10;
    for (size_t i = 0; i < (size_t) tss_shape[0]; i++) {
      int32_t *s_i = ss + i * ss_shape[1];
      char s[100];
      for (size_t k = 0; k < 100; k++) {
        s[k] = (char) s_i[k];
      }

      int32_t *ts = tss + tss_shape[1] * i;
      float *fs = fss + fss_shape[1] * i;
      int32_t *is = iss + iss_shape[1] * i;

      text_color.r = (cs[i] >> 16) & 0xff;
      text_color.g = (cs[i] >> 8) & 0xff;
      text_color.b = cs[i] & 0xff;

      size_t arg_len = 20;
      char* args = malloc(sizeof(char) * arg_len * tss_shape[1]);
      for (size_t j = 0; j < (size_t) tss_shape[1]; j++) {
        char *arg = args + j * arg_len;
        if (ts[j] == 1) {
          snprintf(arg, arg_len, "%.2f", fs[j]);
        } else if (ts[j] == 2) {
          snprintf(arg, arg_len, "%d", is[j]);
        }
      }
      if (tss_shape[1] == 0) {
        snprintf(text, sizeof(text) / sizeof(char), "%s", s);
      } else if (tss_shape[1] == 1) {
        snprintf(text, sizeof(text) / sizeof(char), s, args);
      } else if (tss_shape[1] == 2) {
        snprintf(text, sizeof(text) / sizeof(char), s, args, args + arg_len);
      } else if (tss_shape[1] == 3) {
        snprintf(text, sizeof(text) / sizeof(char),
                 s, args, args + arg_len, args + 2 * arg_len);
      } else if (tss_shape[1] == 4) {
        snprintf(text, sizeof(text) / sizeof(char),
                 s, args, args + arg_len, args + 2 * arg_len, args + 3 * arg_len);
      }
      free(args);
      text_surface = TTF_RenderUTF8_Blended(ctx->font, text, text_color);
      SDL_ASSERT(text_surface != NULL);
      offset_rect.y = y;
      offset_rect.w = text_surface->w;
      offset_rect.h = text_surface->h;
      SDL_ASSERT(SDL_BlitSurface(text_surface, NULL,
                                 ctx->wnd_surface, &offset_rect) == 0);
      SDL_FreeSurface(text_surface);
      y += 30;
    }

    free(ss);
    free(tss);
    free(fss);
    free(iss);
    free(cs);
    FUT_CHECK(ctx->fut, futhark_free_i32_2d(ctx->fut, f_ss));
    FUT_CHECK(ctx->fut, futhark_free_i32_2d(ctx->fut, f_tss));
    FUT_CHECK(ctx->fut, futhark_free_f32_2d(ctx->fut, f_fss));
    FUT_CHECK(ctx->fut, futhark_free_i32_2d(ctx->fut, f_iss));
    FUT_CHECK(ctx->fut, futhark_free_i32_1d(ctx->fut, f_cs));


    SDL_ASSERT(SDL_UpdateWindowSurface(ctx->wnd) == 0);

    SDL_Delay((int) (1000.0 / FPS - delta / 1000));

    handle_sdl_events(ctx);
  }
}

void do_sdl(struct futhark_context *fut, int height, int width) {
  struct lys_context ctx;
  memset(&ctx, 0, sizeof(struct lys_context));

  ctx.last_time = get_wall_time();
  ctx.fut = fut;
  futhark_entry_init(fut, &ctx.state, height, width);

  SDL_ASSERT(SDL_Init(SDL_INIT_EVERYTHING) == 0);
  SDL_ASSERT(TTF_Init() == 0);

  ctx.font = TTF_OpenFont("lib/github.com/diku-dk/lys/Inconsolata-Regular.ttf", 30);
  SDL_ASSERT(ctx.font != NULL);

  ctx.wnd =
    SDL_CreateWindow("Lys",
                     SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
                     width, height, SDL_WINDOW_RESIZABLE);
  SDL_ASSERT(ctx.wnd != NULL);

  window_size_updated(&ctx, width, height);

  ctx.running = 1;
  sdl_loop(&ctx);
  FUT_CHECK(fut, futhark_free_opaque_state(fut, ctx.state));

  free(ctx.data);
  SDL_FreeSurface(ctx.surface);
  TTF_CloseFont(ctx.font);
  // do not free wnd_surface (see SDL_GetWindowSurface)
  SDL_DestroyWindow(ctx.wnd);
  SDL_Quit();
}

void create_futhark_context(const char *deviceopt,
                            struct futhark_context_config **cfg,
                            struct futhark_context **ctx) {
  *cfg = futhark_context_config_new();
  assert(*cfg != NULL);
  futhark_context_config_set_device(*cfg, deviceopt);
  *ctx = futhark_context_new(*cfg);
  assert(*ctx != NULL);

  cl_device_id device;
  assert(clGetCommandQueueInfo(futhark_context_get_command_queue(*ctx),
                               CL_QUEUE_DEVICE, sizeof(cl_device_id), &device, NULL)
         == CL_SUCCESS);

  size_t dev_name_size;
  assert(clGetDeviceInfo(device, CL_DEVICE_NAME, 0, NULL, &dev_name_size)
         == CL_SUCCESS);
  char *dev_name = malloc(dev_name_size);
  assert(clGetDeviceInfo(device, CL_DEVICE_NAME, dev_name_size, dev_name, NULL)
         == CL_SUCCESS);

  printf("Using OpenCL device: %s\n", dev_name);
  free(dev_name);
}

int main(int argc, char** argv) {
  int width = INITIAL_WIDTH, height = INITIAL_HEIGHT;
  char *deviceopt = "";

  int c;

  while ( (c = getopt(argc, argv, "w:h:d:")) != -1) {
    switch (c) {
    case 'w':
      width = atoi(optarg);
      if (width <= 0) {
        fprintf(stderr, "'%s' is not a valid width.\n", optarg);
        exit(EXIT_FAILURE);
      }
      break;
    case 'h':
      height = atoi(optarg);
      if (height <= 0) {
        fprintf(stderr, "'%s' is not a valid width.\n", optarg);
        exit(EXIT_FAILURE);
      }
      break;
    case 'd':
      deviceopt = optarg;
      break;
    default:
      fprintf(stderr, "unknown option: %c\n", c);
      exit(EXIT_FAILURE);
    }
  }

  if (optind < argc) {
    fprintf(stderr, "Excess non-options: ");
    while (optind < argc)
      fprintf(stderr, "%s ", argv[optind++]);
    fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
  }

  struct futhark_context_config* cfg;
  struct futhark_context* ctx;

  create_futhark_context(deviceopt, &cfg, &ctx);
  do_sdl(ctx, height, width);

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
  return 0;
}

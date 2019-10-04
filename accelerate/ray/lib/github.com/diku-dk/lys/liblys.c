// Convenience framework for writing visualisations with Futhark and
// C/SDL.
//
// Based on initial SDL wrapper code by Jakob Stokholm Bertelsen.

#define _XOPEN_SOURCE
#include "liblys.h"
#include PRINTFHEADER

#include <inttypes.h>
#include <assert.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>

#define INITIAL_WIDTH 800
#define INITIAL_HEIGHT 600

#define SDL_ASSERT(x) _sdl_assert(x, __FILE__, __LINE__)
static inline void _sdl_assert(int res, const char *file, int line) {
  if (res == 0) {
    fprintf(stderr, "%s:%d: SDL error %d: %s\n",
            file, line, res, SDL_GetError());
    exit(EXIT_FAILURE);
  }
}

static int64_t get_wall_time(void) {
  struct timeval time;
  assert(gettimeofday(&time,NULL) == 0);
  return time.tv_sec * 1000000 + time.tv_usec;
}

static int font_size_from_dimensions(int width, int height) {
  int size, font_size;
  if (height < width) {
    size = height;
  } else {
    size = width;
  }
  font_size = size / 45;
  if (font_size < 14) {
    font_size = 14;
  } else if (font_size > 32) {
    font_size = 32;
  }
  return font_size;
}

void window_size_updated(struct lys_context *ctx, int newx, int newy) {
  // https://stackoverflow.com/a/40122002
  ctx->wnd_surface = SDL_GetWindowSurface(ctx->wnd);
  SDL_ASSERT(ctx->wnd_surface != NULL);

  ctx->width = newx;
  ctx->height = newy;

  ctx->font_size = font_size_from_dimensions(ctx->width, ctx->height);
  TTF_CloseFont(ctx->font);
  ctx->font = TTF_OpenFont(ctx->font_path, ctx->font_size);
  SDL_ASSERT(ctx->font != NULL);

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
          ctx->show_text = !ctx->show_text;
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

void sdl_loop(struct lys_context *ctx) {
  struct futhark_i32_2d *out_arr;

  while (ctx->running) {
    int64_t now = get_wall_time();
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

    if (ctx->show_text) {
      build_text(ctx, ctx->text_buffer, ctx->text_buffer_len, ctx->text_format,
                 ctx->fps, ctx->sum_names);
      if (*(ctx->text_buffer) != '\0') {
        uint32_t text_colour;
        FUT_CHECK(ctx->fut,
                  futhark_entry_text_colour(ctx->fut, (int32_t*) &text_colour,
                                            ctx->state));
        SDL_Color sdl_text_colour =
          { .a = (text_colour >> 24) & 0xff,
            .r = (text_colour >> 16) & 0xff,
            .g = (text_colour >> 8) & 0xff,
            .b = text_colour & 0xff };

        SDL_Surface *text_surface;
        SDL_Rect offset_rect;
        offset_rect.x = 10;
        int y = 10;
        char* buffer = ctx->text_buffer;
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
            text_surface = TTF_RenderUTF8_Blended(ctx->font, buffer_start, sdl_text_colour);
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
            y += ctx->font_size;
          }
        }
      }
    }

    SDL_ASSERT(SDL_UpdateWindowSurface(ctx->wnd) == 0);

    SDL_Delay((int) (1000.0 / ctx->max_fps - delta / 1000));

    handle_sdl_events(ctx);
  }
}

void do_bench(struct futhark_context *fut, int height, int width, int n, const char *operation) {
  struct futhark_opaque_state *state;
  int64_t start, end;
  FUT_CHECK(fut, futhark_entry_init(fut, &state, (int32_t)get_wall_time(), height, width));
  futhark_context_sync(fut);
  int do_step = 0, do_render = 0;

  if (strstr(operation, "step") != NULL) {
    do_step = 1;
  }

  if (strstr(operation, "render") != NULL) {
    do_render = 1;
  }

  start = get_wall_time();
  for (int i = 0; i < n; i++) {
    if (do_step) {
      struct futhark_opaque_state *new_state;
      FUT_CHECK(fut, futhark_entry_step(fut, &new_state, 1.0/n, state));
      futhark_free_opaque_state(fut, state);
      state = new_state;
    }
    if (do_render) {
      struct futhark_i32_2d *out_arr;
      FUT_CHECK(fut, futhark_entry_render(fut, &out_arr, state));
      FUT_CHECK(fut, futhark_free_i32_2d(fut, out_arr));
    }
  }
  futhark_context_sync(fut);
  end = get_wall_time();

  printf("Rendered %d frames in %fs (%f FPS)\n",
         n, ((double)end-start)/1000000,
         n / (((double)end-start)/1000000));

  FUT_CHECK(fut, futhark_free_opaque_state(fut, state));
}

void do_sdl(struct futhark_context *fut,
            int height, int width, int max_fps,
            bool allow_resize, char* font_path) {
  struct lys_context ctx;
  memset(&ctx, 0, sizeof(struct lys_context));
  ctx.fps = 0;
  ctx.max_fps = max_fps;

  ctx.last_time = get_wall_time();
  ctx.fut = fut;
  futhark_entry_init(fut, &ctx.state, (int32_t)get_wall_time(), height, width);

  SDL_ASSERT(SDL_Init(SDL_INIT_EVERYTHING) == 0);
  SDL_ASSERT(TTF_Init() == 0);

  ctx.font_path = font_path;
  ctx.font_size = font_size_from_dimensions(ctx.width, ctx.height);
  ctx.font = TTF_OpenFont(ctx.font_path, ctx.font_size);
  SDL_ASSERT(ctx.font != NULL);

  int flags = 0;
  if (allow_resize) {
    flags |= SDL_WINDOW_RESIZABLE;
  }
  ctx.wnd =
    SDL_CreateWindow("Lys",
                     SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
                     width, height, flags);
  SDL_ASSERT(ctx.wnd != NULL);

  window_size_updated(&ctx, width, height);

  ctx.running = 1;
  ctx.mouse_grabbed = 0;

  FUT_CHECK(ctx.fut, futhark_entry_grab_mouse(ctx.fut, &ctx.grab_mouse));
  if (ctx.grab_mouse) {
    assert(SDL_SetRelativeMouseMode(1) == 0);
    ctx.mouse_grabbed = 1;
  }

  struct futhark_u8_1d *text_format_array;
  FUT_CHECK(ctx.fut, futhark_entry_text_format(ctx.fut, &text_format_array));
  size_t text_format_len = futhark_shape_u8_1d(ctx.fut, text_format_array)[0];
  ctx.text_format = malloc(sizeof(char) * (text_format_len + 1));
  assert(ctx.text_format != NULL);
  FUT_CHECK(ctx.fut, futhark_values_u8_1d(ctx.fut, text_format_array, (unsigned char*) ctx.text_format));
  ctx.text_format[text_format_len] = '\0';
  FUT_CHECK(ctx.fut, futhark_free_u8_1d(ctx.fut, text_format_array));

  ctx.sum_names = (char* **) malloc(sizeof(char* *) * n_printf_arguments());
  assert(ctx.sum_names != NULL);

  ctx.text_buffer_len = text_format_len;
  size_t i_arg = -1;
  for (size_t i = 0; i < text_format_len; i++) {
    if (ctx.text_format[i] == '%' &&
        i + 1 < text_format_len && ctx.text_format[i + 1] != '%') {
      i_arg++;
      if (ctx.text_format[i + 1] == '[') {
        ctx.text_format[i + 1] = 's';
        size_t end_pos;
        size_t n_choices = 1;
        bool found_end = false;
        for (end_pos = i + 2; end_pos < text_format_len; end_pos++) {
          if (ctx.text_format[end_pos] == '|') {
            n_choices++;
          } else if (ctx.text_format[end_pos] == ']') {
            found_end = true;
            break;
          }
        }
        assert(found_end);
        ctx.sum_names[i_arg] = (char* *) malloc(sizeof(char*) * (n_choices + 1));
        assert(ctx.sum_names[i_arg] != NULL);
        ctx.sum_names[i_arg][n_choices] = NULL;
        char* temp_choice = (char*) malloc(sizeof(char) * (end_pos - i - n_choices));
        assert(temp_choice != NULL);
        size_t choice_cur = 0;
        size_t i_choice = 0;
        for (size_t j = i + 2; j < end_pos + 1; j++) {
          if (ctx.text_format[j] == '|' || ctx.text_format[j] == ']') {
            temp_choice[choice_cur] = '\0';
            ctx.sum_names[i_arg][i_choice] = (char*) malloc(sizeof(char) * (choice_cur + 1));
            assert(ctx.sum_names[i_arg][i_choice] != NULL);
            strncpy(ctx.sum_names[i_arg][i_choice], temp_choice, choice_cur + 1);
            choice_cur = 0;
            i_choice++;
          } else {
            temp_choice[choice_cur] = ctx.text_format[j];
            choice_cur++;
          }
        }
        free(temp_choice);
        size_t shift_left = end_pos - i - 1;
        for (size_t j = end_pos + 1; j < text_format_len; j++) {
          ctx.text_format[j - shift_left] = ctx.text_format[j];
        }
        text_format_len -= shift_left;
        ctx.text_format[text_format_len] = '\0';
        i++;
      } else {
        ctx.sum_names[i_arg] = NULL;
        ctx.text_buffer_len += 20; // estimate
      }
    }
  }

  ctx.text_buffer = malloc(sizeof(char) * ctx.text_buffer_len);
  assert(ctx.text_buffer != NULL);
  ctx.text_buffer[0] = '\0';

  ctx.show_text = 1;

  sdl_loop(&ctx);
  FUT_CHECK(fut, futhark_free_opaque_state(fut, ctx.state));

  free(ctx.text_format);
  free(ctx.text_buffer);

  for (size_t i = 0; i < n_printf_arguments(); i++) {
    if (ctx.sum_names[i] != NULL) {
      size_t j = 0;
      while (ctx.sum_names[i][j] != NULL) {
        free(ctx.sum_names[i][j]);
        j++;
      }
      free(ctx.sum_names[i]);
    }
  }
  free(ctx.sum_names);

  free(ctx.data);
  SDL_FreeSurface(ctx.surface);
  TTF_CloseFont(ctx.font);
  // do not free wnd_surface (see SDL_GetWindowSurface)
  SDL_DestroyWindow(ctx.wnd);
  SDL_Quit();
}

void create_futhark_context(const char *deviceopt,
                            int interactive,
                            struct futhark_context_config **cfg,
                            struct futhark_context **ctx) {
  *cfg = futhark_context_config_new();
  assert(*cfg != NULL);

#if defined(LYS_BACKEND_opencl) || defined(LYS_BACKEND_cuda)
  if (deviceopt != NULL) {
    futhark_context_config_set_device(*cfg, deviceopt);
  }
#else
  (void)deviceopt;
#endif

#ifdef LYS_BACKEND_opencl
  if (interactive) {
    futhark_context_config_select_device_interactively(*cfg);
  }
#else
  (void)interactive;
#endif

  *ctx = futhark_context_new(*cfg);
  assert(*ctx != NULL);

#ifdef LYS_BACKEND_opencl
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
  printf("Use -d or -i to change this.\n");
  free(dev_name);
#endif
}

int main(int argc, char** argv) {
  int width = INITIAL_WIDTH, height = INITIAL_HEIGHT, max_fps = 60;
  bool allow_resize = true;
  char *deviceopt = NULL;
  char *benchopt = NULL;
  int interactive = 0;

  if (argc > 1 && strcmp(argv[1], "--help") == 0) {
    printf("Usage: %s options...\n", argv[0]);
    puts("Options:");
    puts("  -w INT  Set the initial width of the window.");
    puts("  -h INT  Set the initial height of the window.");
    puts("  -R      Disallow resizing the window.");
    puts("  -d DEV  Set the computation device.");
    puts("  -r INT  Maximum frames per second.");
    puts("  -i      Select execution device interactively.");
    puts("  --help  Print this help and exit.");
    return 0;
  }

  int c;
  while ( (c = getopt(argc, argv, "w:h:r:Rd:b:i")) != -1) {
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
    case 'r':
      max_fps = atoi(optarg);
      if (max_fps <= 0) {
        fprintf(stderr, "'%s' is not a valid framerate.\n", optarg);
        exit(EXIT_FAILURE);
      }
      break;
    case 'R':
      allow_resize = false;
      break;
    case 'd':
      deviceopt = optarg;
      break;
    case 'i':
      interactive = 1;
      break;
    case 'b':
      if (strcmp(optarg, "render") == 0 ||
          strcmp(optarg, "step") == 0) {
        benchopt = optarg;
      } else {
        fprintf(stderr, "Use -b <render|step>\n");
        exit(EXIT_FAILURE);
      }
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

  char font_path_rel[] = "/lib/github.com/diku-dk/lys/Inconsolata-Regular.ttf";
  char* font_path = malloc(sizeof(char) * strlen(argv[0]) + sizeof(font_path_rel));
  assert(font_path != NULL);
  strcpy(font_path, argv[0]);
  char *last_dash = strrchr(font_path, '/');
  if (last_dash != NULL) {
    *last_dash = '\0';
  }
  strcat(font_path, font_path_rel);

  struct futhark_context_config* cfg;
  struct futhark_context* ctx;

  create_futhark_context(deviceopt, interactive, &cfg, &ctx);

  if (benchopt != NULL) {
    do_bench(ctx, height, width, max_fps, benchopt);
  } else {
    do_sdl(ctx, height, width, max_fps, allow_resize, font_path);
  }

  free(font_path);

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
  return 0;
}

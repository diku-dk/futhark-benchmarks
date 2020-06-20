#include "liblys.h"
#include PRINTFHEADER

#define _XOPEN_SOURCE
#include <unistd.h>
#include <getopt.h>

#define INITIAL_WIDTH 800
#define INITIAL_HEIGHT 600

struct lys_text {
  TTF_Font *font;
  char* font_path;
  int font_size;
  char* text_format;
  char* text_buffer;
  size_t text_buffer_len;
  bool show_text;
  char* **sum_names;
};

void loop_start(struct lys_context *ctx, struct lys_text *text) {
  struct futhark_u8_1d *text_format_array;
  FUT_CHECK(ctx->fut, futhark_entry_text_format(ctx->fut, &text_format_array));
  size_t text_format_len = futhark_shape_u8_1d(ctx->fut, text_format_array)[0];
  text->text_format = malloc(sizeof(char) * (text_format_len + 1));
  assert(text->text_format != NULL);
  FUT_CHECK(ctx->fut, futhark_values_u8_1d(ctx->fut, text_format_array, (unsigned char*) text->text_format));
  text->text_format[text_format_len] = '\0';
  FUT_CHECK(ctx->fut, futhark_free_u8_1d(ctx->fut, text_format_array));

  text->sum_names = (char* **) malloc(sizeof(char* *) * n_printf_arguments());
  assert(text->sum_names != NULL);

  text->text_buffer_len = text_format_len;
  size_t i_arg = -1;
  for (size_t i = 0; i < text_format_len; i++) {
    if (text->text_format[i] == '%' &&
        i + 1 < text_format_len && text->text_format[i + 1] != '%') {
      i_arg++;
      if (text->text_format[i + 1] == '[') {
        text->text_format[i + 1] = 's';
        size_t end_pos;
        size_t n_choices = 1;
        bool found_end = false;
        for (end_pos = i + 2; end_pos < text_format_len; end_pos++) {
          if (text->text_format[end_pos] == '|') {
            n_choices++;
          } else if (text->text_format[end_pos] == ']') {
            found_end = true;
            break;
          }
        }
        assert(found_end);
        text->sum_names[i_arg] = (char* *) malloc(sizeof(char*) * (n_choices + 1));
        assert(text->sum_names[i_arg] != NULL);
        text->sum_names[i_arg][n_choices] = NULL;
        char* temp_choice = (char*) malloc(sizeof(char) * (end_pos - i - n_choices));
        assert(temp_choice != NULL);
        size_t choice_cur = 0;
        size_t i_choice = 0;
        for (size_t j = i + 2; j < end_pos + 1; j++) {
          if (text->text_format[j] == '|' || text->text_format[j] == ']') {
            temp_choice[choice_cur] = '\0';
            text->sum_names[i_arg][i_choice] = (char*) malloc(sizeof(char) * (choice_cur + 1));
            assert(text->sum_names[i_arg][i_choice] != NULL);
            strncpy(text->sum_names[i_arg][i_choice], temp_choice, choice_cur + 1);
            choice_cur = 0;
            i_choice++;
          } else {
            temp_choice[choice_cur] = text->text_format[j];
            choice_cur++;
          }
        }
        free(temp_choice);
        size_t shift_left = end_pos - i - 1;
        for (size_t j = end_pos + 1; j < text_format_len; j++) {
          text->text_format[j - shift_left] = text->text_format[j];
        }
        text_format_len -= shift_left;
        text->text_format[text_format_len] = '\0';
        i++;
      } else {
        text->sum_names[i_arg] = NULL;
        text->text_buffer_len += 20; // estimate
      }
    }
  }

  text->text_buffer = malloc(sizeof(char) * text->text_buffer_len);
  assert(text->text_buffer != NULL);
  text->text_buffer[0] = '\0';

  text->show_text = true;
}

void loop_iteration(struct lys_context *ctx, struct lys_text *text) {
  if (!text->show_text) {
    return;
  }

  build_text(ctx, text->text_buffer, text->text_buffer_len, text->text_format,
             ctx->fps, text->sum_names);
  if (*(text->text_buffer) != '\0') {
    int32_t text_colour;
    FUT_CHECK(ctx->fut,
              futhark_entry_text_colour(ctx->fut, (uint32_t*) &text_colour,
                                        ctx->state));
    draw_text(ctx, text->font, text->font_size, text->text_buffer, text_colour, 10, 10);
  }
}

void loop_end(struct lys_text *text) {
  free(text->text_format);
  free(text->text_buffer);

  for (size_t i = 0; i < n_printf_arguments(); i++) {
    if (text->sum_names[i] != NULL) {
      size_t j = 0;
      while (text->sum_names[i][j] != NULL) {
        free(text->sum_names[i][j]);
        j++;
      }
      free(text->sum_names[i]);
    }
  }
  free(text->sum_names);
}

int font_size_from_dimensions(int width, int height) {
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

void window_size_updated(struct lys_context *ctx, struct lys_text *text) {
  text->font_size = font_size_from_dimensions(ctx->width, ctx->height);
  TTF_CloseFont(text->font);
  text->font = TTF_OpenFont(text->font_path, text->font_size);
  SDL_ASSERT(text->font != NULL);
}

void f1(struct lys_text *text) {
  text->show_text = !text->show_text;
}

void handle_event(struct lys_context *ctx, enum lys_event event) {
  struct lys_text *text = (struct lys_text *) ctx->event_handler_data;
  switch (event) {
  case LYS_LOOP_START:
    loop_start(ctx, text);
    break;
  case LYS_LOOP_ITERATION:
    loop_iteration(ctx, text);
    break;
  case LYS_LOOP_END:
    loop_end(text);
    break;
  case LYS_WINDOW_SIZE_UPDATED:
    window_size_updated(ctx, text);
    break;
  case LYS_F1:
    f1(text);
  }
}

void do_bench(struct futhark_context *fut, int height, int width, int n, const char *operation) {
  struct futhark_opaque_state *state;
  int64_t start, end;
  FUT_CHECK(fut, futhark_entry_init(fut, &state, (int32_t) lys_wall_time(), height, width));
  futhark_context_sync(fut);
  bool do_step = false, do_render = false;

  if (strstr(operation, "step") != NULL) {
    do_step = true;
  }

  if (strstr(operation, "render") != NULL) {
    do_render = true;
  }

  start = lys_wall_time();
  for (int i = 0; i < n; i++) {
    if (do_step) {
      struct futhark_opaque_state *new_state;
      FUT_CHECK(fut, futhark_entry_step(fut, &new_state, 1.0/n, state));
      futhark_free_opaque_state(fut, state);
      state = new_state;
    }
    if (do_render) {
      struct futhark_u32_2d *out_arr;
      FUT_CHECK(fut, futhark_entry_render(fut, &out_arr, state));
      FUT_CHECK(fut, futhark_free_u32_2d(fut, out_arr));
    }
  }
  futhark_context_sync(fut);
  end = lys_wall_time();

  printf("Rendered %d frames in %fs (%f FPS)\n",
         n, ((double)end-start)/1000000,
         n / (((double)end-start)/1000000));

  FUT_CHECK(fut, futhark_free_opaque_state(fut, state));
}

void usage(char **argv) {
  printf("Usage: %s options...\n", argv[0]);
  puts("Options:");
  puts("  -?      Print this help and exit.");
  puts("  -w INT  Set the initial width of the window.");
  puts("  -h INT  Set the initial height of the window.");
  puts("  -R      Disallow resizing the window.");
  puts("  -d DEV  Set the computation device.");
  puts("  -r INT  Maximum frames per second.");
  puts("  -i      Select execution device interactively.");
  puts("  -b <render|step>  Benchmark program.");
}

int main(int argc, char** argv) {
  int width = INITIAL_WIDTH, height = INITIAL_HEIGHT, max_fps = 60;
  bool allow_resize = true;
  char *deviceopt = NULL;
  bool device_interactive = false;
  char *benchopt = NULL;

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
      device_interactive = true;
      break;
    case 'b':
      if (strcmp(optarg, "render") == 0 ||
          strcmp(optarg, "step") == 0) {
        benchopt = optarg;
      } else {
        fprintf(stderr, "Use -b <render|step>\n");
        return EXIT_FAILURE;
      }
      break;
    case '?':
      usage(argv);
      return EXIT_SUCCESS;
    default:
      fprintf(stderr, "unknown option: %c\n", c);
      usage(argv);
      return EXIT_FAILURE;
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

  int sdl_flags = 0;
  if (allow_resize) {
    sdl_flags |= SDL_WINDOW_RESIZABLE;
  }

  struct lys_context ctx;
  struct futhark_context_config *futcfg;
  lys_setup(&ctx, width, height, max_fps, sdl_flags);

  char* opencl_device_name = NULL;
  lys_setup_futhark_context(deviceopt, device_interactive,
                            &futcfg, &ctx.fut, &opencl_device_name);
  if (opencl_device_name != NULL) {
    printf("Using OpenCL device: %s\n", opencl_device_name);
    printf("Use -d or -i to change this.\n");
    free(opencl_device_name);
  }

  FUT_CHECK(ctx.fut, futhark_entry_grab_mouse(ctx.fut, &ctx.grab_mouse));

  struct lys_text text;
  ctx.event_handler_data = (void*) &text;
  ctx.event_handler = handle_event;

  SDL_ASSERT(TTF_Init() == 0);

  text.font_path = font_path;
  text.font_size = font_size_from_dimensions(ctx.width, ctx.height);
  text.font = TTF_OpenFont(text.font_path, text.font_size);
  SDL_ASSERT(text.font != NULL);

  if (benchopt != NULL) {
    do_bench(ctx.fut, height, width, max_fps, benchopt);
  } else {
    int32_t seed = (int32_t) lys_wall_time();
    futhark_entry_init(ctx.fut, &ctx.state,
                       seed, ctx.height, ctx.width);
    lys_run_sdl(&ctx);
    free(ctx.data);
  }

  TTF_CloseFont(text.font);
  free(font_path);

  futhark_context_free(ctx.fut);
  futhark_context_config_free(futcfg);

  return EXIT_SUCCESS;
}

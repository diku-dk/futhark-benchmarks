#include "liblys.h"
#include PRINTFHEADER

#include <unistd.h>
#include <getopt.h>
#include <string.h>

#define INITIAL_WIDTH 800
#define INITIAL_HEIGHT 600

void loop_start(struct lys_context *ctx, struct lys_text *text) {
  prepare_text(ctx->fut, text);
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
    draw_text(ctx, text->text_buffer, text_colour, 1, 1);
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
    break;
  case LYS_F1:
    f1(text);
  }
}

void usage(char **argv) {
  printf("Usage: %s options...\n", argv[0]);
  puts("Options:");
  puts("  -?      Print this help and exit.");
  puts("  -d DEV  Set the computation device.");
  puts("  -r INT  Maximum frames per second.");
  puts("  -i      Select execution device interactively.");
  puts("  -b <render|step>  Benchmark program.");
}

int main(int argc, char** argv) {
  int max_fps = 60;
  char *deviceopt = NULL;
  bool device_interactive = false;

  int c;
  while ( (c = getopt(argc, argv, "r:d:i")) != -1) {
    switch (c) {
    case 'r':
      max_fps = atoi(optarg);
      if (max_fps <= 0) {
        fprintf(stderr, "'%s' is not a valid framerate.\n", optarg);
        exit(EXIT_FAILURE);
      }
      break;
    case 'd':
      deviceopt = optarg;
      break;
    case 'i':
      device_interactive = true;
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

  void* buf = malloc(1024*1024);
  setvbuf(stdout, buf, _IOFBF, 1024*1024);


  if (optind < argc) {
    fprintf(stderr, "Excess non-options: ");
    while (optind < argc)
      fprintf(stderr, "%s ", argv[optind++]);
    fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
  }

  struct lys_context ctx;
  struct futhark_context_config *futcfg;
  lys_setup(&ctx, max_fps);

  char* opencl_device_name = NULL;
  lys_setup_futhark_context(deviceopt, device_interactive,
                            &futcfg, &ctx.fut, &opencl_device_name);
  if (opencl_device_name != NULL) {
    free(opencl_device_name);
  }

  struct lys_text text;
  ctx.event_handler_data = &text;
  ctx.event_handler = handle_event;

  int32_t seed = (int32_t) lys_wall_time();
  futhark_entry_init(ctx.fut, &ctx.state, seed, ctx.height, ctx.width);
  lys_run_ncurses(&ctx);

  futhark_context_free(ctx.fut);
  futhark_context_config_free(futcfg);

  return EXIT_SUCCESS;
}

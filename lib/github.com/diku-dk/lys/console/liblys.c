#include "liblys.h"
#include <sys/ioctl.h>
#include <unistd.h>
#include <string.h>

struct termios orig_termios;

void cooked_mode() {
  tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
  printf("\033[?25h");
}

void raw_mode() {
  printf("\033[?25l");

  tcgetattr(STDIN_FILENO, &orig_termios);
  atexit(cooked_mode);

  struct termios raw = orig_termios;
  raw.c_iflag &= ~(IXON);
  raw.c_lflag &= ~(ECHO | ICANON | ISIG);
  raw.c_cc[VMIN] = 0;
  raw.c_cc[VTIME] = 0;
  tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}


void def() {
  printf("\033[0m");
}

void fg_rgb(FILE *f, uint8_t r, uint8_t g, uint8_t b) {
  fprintf(f, "\033[38;2;%d;%d;%dm", r, g, b);
}

void bg_rgb(FILE *f, uint8_t r, uint8_t g, uint8_t b) {
  fprintf(f, "\033[48;2;%d;%d;%dm", r, g, b);
}

void cursor_goto(int x, int y) {
  printf("\033[%d;%dH", y, x);
}

void cursor_home() {
  printf("\033[;H");
}

void render(int nrows, int ncols, const uint32_t *rgbs,
            uint32_t *fgs, uint32_t *bgs, char *chars) {
  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      uint32_t w0 = rgbs[(i*2)*ncols+j];
      uint32_t w1 = rgbs[(i*2+1)*ncols+j];
      fgs[i*ncols+j] = w0;
      bgs[i*ncols+j] = w1;
      chars[i*ncols+j] = 127; // Sentinel.
    }
  }
}

void display(FILE *f, bool eol, int nrows, int ncols,
             const uint32_t *fgs, const uint32_t *bgs, const char *chars) {
  uint32_t prev_w0 = 0xdeadbeef;
  uint32_t prev_w1 = 0xdeadbeef;
  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      double r0 = 0, g0 = 0, b0 = 0;
      double r1 = 0, g1 = 0, b1 = 0;
      uint32_t w0 = fgs[i*ncols+j];
      uint32_t w1 = bgs[i*ncols+j];
      if (w0 != prev_w0 || w1 != prev_w1) {
        r0 = (w0>>16)&0xFF;
        g0 = (w0>>8)&0xFF;
        b0 = (w0>>0)&0xFF;
        r1 = (w1>>16)&0xFF;
        g1 = (w1>>8)&0xFF;
        b1 = (w1>>0)&0xFF;
        fg_rgb(f, r0, g0, b0);
        bg_rgb(f, r1, g1, b1);
        prev_w0 = w0;
        prev_w1 = w1;
      }
      char c = chars[i*ncols+j];
      if (c == 127) {
        fputs("▀", f);
      } else {
        fputc(c, f);
      }
    }
    if (eol) {
      fputc('\n', f);
    }
  }
}

void keydown(struct lys_context *ctx, int keysym) {
  ctx->key_pressed = keysym;
  struct futhark_opaque_state *new_state;
  FUT_CHECK(ctx->fut, futhark_entry_key(ctx->fut, &new_state, 0, keysym, ctx->state));
  futhark_free_opaque_state(ctx->fut, ctx->state);
  ctx->state = new_state;
}


void keyup(struct lys_context *ctx, int keysym) {
  struct futhark_opaque_state *new_state;
  FUT_CHECK(ctx->fut, futhark_entry_key(ctx->fut, &new_state, 1, keysym, ctx->state));
  futhark_free_opaque_state(ctx->fut, ctx->state);
  ctx->state = new_state;
}

void get_terminal_size(int* nrows, int* ncols) {
  struct winsize w;
  ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);
  *nrows = w.ws_row;
  *ncols = w.ws_col;
}

void resize(struct lys_context *ctx) {
  int nrows, ncols;
  get_terminal_size(&nrows, &ncols);
  ctx->width = ncols;
  ctx->height = nrows*2;
  ctx->fgs = realloc(ctx->fgs, nrows*ncols*sizeof(uint32_t));
  ctx->bgs = realloc(ctx->bgs, nrows*ncols*sizeof(uint32_t));
  ctx->chars = realloc(ctx->chars, nrows*ncols*sizeof(char));
  ctx->rgbs = realloc(ctx->rgbs, ctx->width*ctx->height*sizeof(uint32_t));

  struct futhark_opaque_state *new_state;
  FUT_CHECK(ctx->fut, futhark_entry_resize(ctx->fut, &new_state, ctx->height, ctx->width, ctx->state));
  futhark_free_opaque_state(ctx->fut, ctx->state);
  ctx->state = new_state;
}

void maybe_resize(struct lys_context *ctx) {
  int nrows, ncols;
  get_terminal_size(&nrows, &ncols);

  if (nrows != ctx->width || ncols*2 != ctx->height) {
    resize(ctx);
  }
}

// Best-effort at translating VT100 key codes to SDL.
//
// The handling of keydown/keyup events is complicated by the fact
// that the terminal does not report keyup events.  As a workaround,
// we treat every input key as a keydown for one frame, then a keyup
// the following frame.  Many applications will misbehave, but not
// all!
void check_input(struct lys_context *ctx) {
  if (ctx->key_pressed) {
    keyup(ctx, ctx->key_pressed);
    ctx->key_pressed = 0;
  }

  char c;
  if (read(STDIN_FILENO, &c, 1) != 0) {
    switch (c) {
    case 3: // Ctrl-c
      ctx->running = 0;
      return;
    case 0x1b: // Escape
      if (read(STDIN_FILENO, &c, 1) != 0) {
        switch (c) {
        case 0x1b: // Double escape!
          ctx->running = 0;
          return;
        case 'O': // Application key
          if (read(STDIN_FILENO, &c, 1) != 0) {
            switch (c) {
            case 'P':
              keydown(ctx, 0x4000003A);
              return;
            case 'Q':
              keydown(ctx, 0x4000003B);
              return;
            case 'R':
              keydown(ctx, 0x4000003C);
              return;
            case 'S':
              keydown(ctx, 0x4000003D);
              return;
            }
          }
          return;
        }
      }
      if (read(STDIN_FILENO, &c, 1) != 0) {
        switch (c) {
        case 'A':
          // Arrow up
          keydown(ctx, 0x40000052);
          return;
        case 'B':
          // Arrow down
          keydown(ctx, 0x40000051);
          return;
        case 'C':
          // Arrow right
          keydown(ctx, 0x4000004F);
          return;
        case 'D':
          // Arrow left
          keydown(ctx, 0x40000050);
          return;
        }
      }
      break;
    default:
      if (c >= 'a' && c <= 'z') {
        keydown(ctx, 0x61 + (c-'a'));
        return;
      }
      if (c >= '0' && c <= '9') {
        keydown(ctx, 0x30 + (c-'0'));
        return;
      }
    }
  }
}

void lys_run_console(struct lys_context *ctx) {
  ctx->running = 1;
  ctx->last_time = lys_wall_time();

  int num_frames = 0;

  ctx->event_handler(ctx, LYS_LOOP_START);

  while (ctx->running && ctx->num_frames-- > 0) {
    num_frames++;
    int64_t now = lys_wall_time();
    float delta;
    if (ctx->interactive) {
      delta = ((float)(now - ctx->last_time))/1000000.0;
    } else {
      delta = 1/(double)ctx->max_fps;
    }
    ctx->fps = (ctx->fps*0.9 + (1/delta)*0.1);
    ctx->last_time = now;
    struct futhark_opaque_state *new_state, *old_state = ctx->state;
    FUT_CHECK(ctx->fut, futhark_entry_step(ctx->fut, &new_state, delta, old_state));
    ctx->state = new_state;

    struct futhark_u32_2d *out_arr;
    FUT_CHECK(ctx->fut, futhark_entry_render(ctx->fut, &out_arr, ctx->state));
    FUT_CHECK(ctx->fut, futhark_values_u32_2d(ctx->fut, out_arr, ctx->rgbs));
    FUT_CHECK(ctx->fut, futhark_context_sync(ctx->fut));
    FUT_CHECK(ctx->fut, futhark_free_u32_2d(ctx->fut, out_arr));
    FUT_CHECK(ctx->fut, futhark_free_opaque_state(ctx->fut, old_state));

    {
      // Ideally we should only check for resize if WIGWINCH has been
      // received, but this ioctl is pretty fast anyway.
      maybe_resize(ctx);
      int nrows = ctx->height/2;
      int ncols = ctx->width;
      render(nrows, ncols, ctx->rgbs, ctx->fgs, ctx->bgs, ctx->chars);
      ctx->event_handler(ctx, LYS_LOOP_ITERATION);
      if (ctx->interactive) {
        cursor_goto(0,0);
      }
      display(ctx->out, !ctx->interactive, nrows, ncols, ctx->fgs, ctx->bgs, ctx->chars);
    }
    if (ctx->interactive) {
      fflush(stdout);
      check_input(ctx);

      int delay =  1000.0/ctx->max_fps - delta*1000.0;
      if (delay > 0) {
        usleep(delay*1000);
      }

      def();
    }
  }

  ctx->event_handler(ctx, LYS_LOOP_END);

  // Our cleanup.
  free(ctx->rgbs);
  free(ctx->fgs);
  free(ctx->bgs);
  free(ctx->chars);
  FUT_CHECK(ctx->fut, futhark_free_opaque_state(ctx->fut, ctx->state));
}

void lys_setup(struct lys_context *ctx, int max_fps, int num_frames, FILE* out, int width, int height) {
  memset(ctx, 0, sizeof(struct lys_context));

  ctx->fps = 0;
  ctx->max_fps = max_fps;
  ctx->num_frames = num_frames;
  ctx->interactive = out == NULL;

  if (ctx->interactive) {
    int nrows, ncols;
    get_terminal_size(&nrows, &ncols);
    assert(nrows >= 0 && ncols >= 0);
    raw_mode();
    ctx->width = ncols;
    ctx->height = nrows*2;
    ctx->out = stdout;
  } else {
    ctx->out = out;
    ctx->width = width;
    ctx->height = height;
  }

  ctx->fgs = malloc(ctx->width * ctx->height * sizeof(uint32_t));
  ctx->bgs = malloc(ctx->width * ctx->height * sizeof(uint32_t));
  ctx->chars = malloc(ctx->width * ctx->height * sizeof(char));
  ctx->rgbs = malloc(ctx->width*ctx->height*sizeof(uint32_t));
  ctx->key_pressed = 0;
}

void draw_text(struct lys_context *ctx, char* buffer, int32_t colour,
               int x_start, int y_start) {
  int x = x_start;
  int y = y_start;
  for (int i = 0; buffer[i]; i++) {
    if (buffer[i] == '\n') {
      x = x_start;
      y++;
      continue;
    } else {
      if (x < ctx->width && y < ctx->height) {
        ctx->fgs[y*ctx->width+x] = colour;
        ctx->bgs[y*ctx->width+x] = ~colour;
        ctx->chars[y*ctx->width+x] = buffer[i];
      }
      x++;
    }
  }
}

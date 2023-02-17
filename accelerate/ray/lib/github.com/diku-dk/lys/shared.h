#ifndef LIBLYS_SHARED
#define LIBLYS_SHARED

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>
#include <sys/time.h>

#include PROGHEADER

void lys_setup_futhark_context(const char *deviceopt, bool device_interactive,
                               struct futhark_context_config* *futcfg,
                               struct futhark_context* *futctx,
                               char* *opencl_device_name);

int64_t lys_wall_time();

#define FUT_CHECK(ctx, x) _fut_check(ctx, x, __FILE__, __LINE__)
static inline void _fut_check(struct futhark_context *ctx, int res,
                              const char *file, int line) {
  if (res != 0) {
    fprintf(stderr, "%s:%d: Futhark error %d: %s\n",
            file, line, res, futhark_context_get_error(ctx));
    exit(EXIT_FAILURE);
  }
}

#ifdef LYS_TEXT
struct lys_text {
  char* text_format;
  char* text_buffer;
  size_t text_buffer_len;
  bool show_text;
  char* **sum_names;
};

void prepare_text(struct futhark_context* futctx, struct lys_text *text);
#endif

#endif

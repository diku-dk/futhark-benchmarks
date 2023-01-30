// Convert input data from Futhark format to PBBS to the Futhark
// binary data format.  Accepts data on stdin and produces results on
// stdout.  May not support all PBBS data formats yet, but should be
// enough for the currently implemented benchmarks.

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char** argv) {
  if (argc != 1) {
    fprintf(stderr, "%s takes no options.\n", argv[0]);
    exit(1);
  }

  if (isatty(fileno(stdin))) {
    fprintf(stderr, "%s: stdin is a tty - redirect from a file instead.\n",
            argv[0]);
    exit(1);
  }

  if (isatty(fileno(stdout))) {
    fprintf(stderr, "%s: stdout is a tty - you probably want to redirect to a file instead.\n",
            argv[0]);
  }

  if (getc(stdin) != 'b' || getc(stdin) != 2) {
    fprintf(stderr, "%s: invalid input.\n", argv[0]);
    exit(1);
  }

  uint8_t num_dims;
  char type[4];

  if (fread(&num_dims, sizeof(num_dims), 1, stdin) != 1) {
    fprintf(stderr, "%s: could not read number of dimensions\n", argv[0]);
    exit(1);
  }

  if (fread(&type, sizeof(type), 1, stdin) != 1) {
    fprintf(stderr, "%s: could not read type\n", argv[0]);
    exit(1);
  }

  int64_t shape[num_dims];

  if (fread(&shape, sizeof(int64_t), num_dims, stdin) != num_dims) {
    fprintf(stderr, "%s: could not read shape\n", argv[0]);
    exit(1);
  }

  if (memcmp(type, " i32", 4) == 0 && num_dims == 1) {
    fprintf(stdout, "sequenceInt\n");
    for (int i = 0; i < shape[0]; i++) {
      int32_t x;
      if (fread(&x, sizeof(x), 1, stdin) != 1) {
        fprintf(stderr, "%s: failed to read all values\n", argv[0]);
        exit(1);
      }
      fprintf(stdout, "%d\n", x);
    }
  } else if (memcmp(type, " i64", 4) == 0 && num_dims == 1) {
    fprintf(stdout, "sequenceInt\n");
    for (int i = 0; i < shape[0]; i++) {
      int64_t x;
      if (fread(&x, sizeof(x), 1, stdin) != 1) {
        fprintf(stderr, "%s: failed to read all values\n", argv[0]);
        exit(1);
      }
      fprintf(stdout, "%ld\n", x);
    }
  } else if (memcmp(type, " f64", 4) == 0 && num_dims == 2 && shape[1] == 2) {
    fprintf(stdout, "pbbs_sequencePoint2d\n");
    for (int i = 0; i < shape[0]; i++) {
      double xs[2];
      if (fread(&xs, sizeof(xs), 1, stdin) != 1) {
        fprintf(stderr, "%s: failed to read all values\n", argv[0]);
        exit(1);
      }
      // Winging the precision here; hopefully will be OK.
      fprintf(stdout, "%.20f %.20f\n", xs[0], xs[1]);
    }
  } else {
    fprintf(stderr, "%s: cannot handle value with shape ", argv[0]);
    for (int i = 0; i < num_dims; i++) {
      fprintf(stderr, "[%ld]", (long)shape[i]);
    }
    fprintf(stderr, " and type %c%c%c%c.\n",
            type[0], type[1], type[2], type[3]);
  }
}

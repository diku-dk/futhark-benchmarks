// Convert input data from Futhark binary format to PBBS.  Accepts
// data on stdin and produces results on stdout.  May not support all
// PBBS data formats yet, but should be enough for the currently
// implemented benchmarks.
//
// Usage: fut2pbbs < input > output

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

struct type {
  char type[4];
  uint8_t rank;
  int64_t shape[256];
  long data_offset;
};

char* progname;

int64_t bytes(const char type[4]) {
  if (memcmp(type, "  u8", 4) == 0 || memcmp(type, "  i8", 4) == 0 || memcmp(type, "bool", 4) == 0) {
    return 1;
  }
  if (memcmp(type, " u16", 4) == 0 || memcmp(type, " i16", 4) == 0 || memcmp(type, " f16", 4) == 0) {
    return 2;
  }
  if (memcmp(type, " u32", 4) == 0 || memcmp(type, " i32", 4) == 0 || memcmp(type, " f32", 4) == 0) {
    return 4;
  }
  if (memcmp(type, " u64", 4) == 0 || memcmp(type, " i64", 4) == 0 || memcmp(type, " f64", 4) == 0) {
    return 8;
  }
  fprintf(stderr, "%s: unknown type name: %c%c%c%c.\n", progname, type[0], type[1], type[2], type[3]);
  exit(1);
}

void skipspaces(FILE* f) {
  int c;
  while ((c = fgetc(f)) != EOF) {
    if (!isspace(c)) {
      ungetc(c, f);
      return;
    }
  }
}

// Find the types of values in the file.
int read_types(FILE* f, struct type* ts) {
  int i = 0;
  while (1) {
    skipspaces(f);
    if (fgetc(f) != 'b' || fgetc(f) != 2) {
      if (feof(f)) {
        break;
      } else {
        fprintf(stderr, "%s: value %d: invalid input.\n", progname, i);
        exit(1);
      }
    }
    if (fread(&ts[i].rank, sizeof(uint8_t), 1, f) != 1) {
      fprintf(stderr, "%s: value %d: could not read number of dimensions\n", progname, i);
      exit(1);
    }
    if (fread(&ts[i].type, 4, 1, f) != 1) {
      fprintf(stderr, "%s: value %d: could not read type\n", progname, i);
      exit(1);
    }
    if (fread(&ts[i].shape, sizeof(int64_t), ts[i].rank, f) != ts[i].rank) {
      fprintf(stderr, "%s: value %d: could not read shape\n", progname, i);
      exit(1);
    }
    ts[i].data_offset = ftell(f);
    long num_elems = 1;
    for (int j = 0; j < ts[i].rank; j++) {
      num_elems *= ts[i].shape[j];
    }
    fseek(f, num_elems*bytes(ts[i].type), SEEK_CUR);
    i++;
  }

  return i;
}

int main(int argc, char** argv) {
  progname = argv[0];

  if (argc != 1) {
    fprintf(stderr, "%s takes no options.\n", argv[0]);
    exit(1);
  }

  if (lseek(fileno(stdin), 0, SEEK_SET) == -1) {
    fprintf(stderr, "%s: stdin is not a file - redirect from a file instead.\n",
            argv[0]);
    exit(1);
  }

  if (isatty(fileno(stdout))) {
    fprintf(stderr, "%s: stdout is a tty - you probably want to redirect to a file instead.\n",
            argv[0]);
  }

  struct type types[16]; // Hopefully enough.

  int num_values = read_types(stdin, types);
  if (num_values == 1 && memcmp(types[0].type, " i32", 4) == 0 && types[0].rank == 1) {
    fseek(stdin, types[0].data_offset, SEEK_SET);
    fprintf(stdout, "sequenceInt\n");
    for (int i = 0; i < types[0].shape[0]; i++) {
      int32_t x;
      if (fread(&x, sizeof(x), 1, stdin) != 1) {
        fprintf(stderr, "%s: failed to read all values\n", progname);
        exit(1);
      }
      fprintf(stdout, "%d\n", x);
    }
  } else if (num_values == 1 && memcmp(types[0].type, " i64", 4) == 0 && types[0].rank == 1) {
    fseek(stdin, types[0].data_offset, SEEK_SET);
    fprintf(stdout, "sequenceInt\n");
    for (int i = 0; i < types[0].shape[0]; i++) {
      int64_t x;
      if (fread(&x, sizeof(x), 1, stdin) != 1) {
        fprintf(stderr, "%s: failed to read all values\n", argv[0]);
        exit(1);
      }
      fprintf(stdout, "%ld\n", x);
    }
  } else if (num_values == 1 && memcmp(types[0].type, " f64", 4) == 0 && types[0].rank == 1) {
    fseek(stdin, types[0].data_offset, SEEK_SET);
    fprintf(stdout, "pbbs_sequenceDouble\n");
    for (int i = 0; i < types[0].shape[0]; i++) {
      double x;
      if (fread(&x, sizeof(x), 1, stdin) != 1) {
        fprintf(stderr, "%s: failed to read all values\n", argv[0]);
        exit(1);
      }
      // Winging the precision here; hopefully will be OK.
      fprintf(stdout, "%.20f\n", x);
    }
  } else if (num_values == 1 && memcmp(types[0].type, " f64", 4) == 0 && types[0].rank == 2 && types[0].shape[1] == 2) {
    fseek(stdin, types[0].data_offset, SEEK_SET);
    fprintf(stdout, "pbbs_sequencePoint2d\n");
    for (int i = 0; i < types[0].shape[0]; i++) {
      double xs[2];
      if (fread(&xs, sizeof(xs), 1, stdin) != 1) {
        fprintf(stderr, "%s: failed to read all values\n", argv[0]);
        exit(1);
      }
      // Winging the precision here; hopefully will be OK.
      fprintf(stdout, "%.20f %.20f\n", xs[0], xs[1]);
    }
  } else if (num_values == 2 &&
             memcmp(types[0].type, " i32", 2) == 0 && types[0].rank == 1 &&
             memcmp(types[1].type, " i32", 2) == 0 && types[1].rank == 1) {
    fprintf(stdout, "AdjacencyGraph\n");
    fprintf(stdout, "%ld\n", types[0].shape[0]);
    fprintf(stdout, "%ld\n", types[1].shape[0]);
    fseek(stdin, types[0].data_offset, SEEK_SET);
    for (int i = 0; i < types[0].shape[0]; i++) {
      int32_t x;
      if (fread(&x, sizeof(x), 1, stdin) != 1) {
        fprintf(stderr, "%s: failed to read all values\n", progname);
        exit(1);
      }
      fprintf(stdout, "%d\n", x);
    }
    fseek(stdin, types[1].data_offset, SEEK_SET);
    for (int i = 0; i < types[1].shape[0]; i++) {
      int32_t x;
      if (fread(&x, sizeof(x), 1, stdin) != 1) {
        fprintf(stderr, "%s: failed to read all values\n", progname);
        exit(1);
      }
      fprintf(stdout, "%d\n", x);
    }
  } else if (num_values == 2 &&
             memcmp(types[0].type, " i32", 2) == 0 && types[0].rank == 2 &&
             memcmp(types[1].type, " f64", 2) == 0 && types[1].rank == 1) {
    fprintf(stdout, "WeightedEdgeArray\n");

    int32_t (*buffer)[2] = malloc(sizeof(int32_t[types[0].shape[0]][2]));

    fseek(stdin, types[0].data_offset, SEEK_SET);
    for (int i = 0; i < types[0].shape[0]; i++) {
      int32_t x;
      if (fread(&x, sizeof(x), 1, stdin) != 1) {
        fprintf(stderr, "%s: failed to read all values\n", progname);
        exit(1);
      }
      buffer[i][0] = x;

      int32_t x2;
      if (fread(&x2, sizeof(x2), 1, stdin) != 1) {
        fprintf(stderr, "%s: failed to read all values\n", progname);
        exit(1);
      }
      buffer[i][1] = x2;
    }
    fseek(stdin, types[1].data_offset, SEEK_SET);
    for (int i = 0; i < types[1].shape[0]; i++) {
      double f;
      if (fread(&f, sizeof(f), 1, stdin) != 1) {
        fprintf(stderr, "%s: failed to read all values\n", progname);
        exit(1);
      }
      fprintf(stdout, "%d %d %.20f\n", buffer[i][0], buffer[i][1], f);
    }
  } else {
    fprintf(stderr, "%s: cannot handle file with %d values of these types:\n", argv[0], num_values);
    for (int i = 0; i < num_values; i++) {
      for (int j = 0; j < types[i].rank; j++) {
        fprintf(stderr, "[%ld]", (long)types[i].shape[j]);
      }
      fprintf(stderr, "%c%c%c%c.\n", types[i].type[0], types[i].type[1], types[i].type[2], types[i].type[3]);
    }
    return 1;
  }
}

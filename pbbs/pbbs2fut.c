// Convert input data from the PBBS format to the Futhark binary data
// format.  Accepts data on stdin and produces results on stdout.  May
// not support all PBBS data formats yet, but should be enough for the
// currently implemented benchmarks.

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>

void header(FILE *out, uint8_t num_dims, const char *type, uint64_t *dims) {
  uint8_t header = 'b';
  uint8_t version = 2;
  fwrite(&header, 1, 1, out);
  fwrite(&version, 1, 1, out);
  fwrite(&num_dims, 1, 1, out);
  fwrite(type, 4, 1, out);
  for (int i = 0; i < num_dims; i++) {
    fwrite(&dims[i], sizeof(uint64_t), 1, out);
  }
}

void conv_float(FILE *in, FILE *out) {
  float f;
  int ret = fscanf(in, "%f", &f);
  assert(ret == 1);
  fwrite(&f, sizeof(float), 1, out);
}

void conv_i32(FILE *in, FILE *out) {
  int32_t x;
  int ret = fscanf(in, "%d", &x);
  assert(ret == 1);
  fwrite(&x, sizeof(int), 1, out);
}

void sequenceInt(FILE *in, FILE *out) {
  int used = 0, capacity = 100;
  int32_t *data = malloc(capacity*sizeof(int32_t));
  while (fscanf(in, "%d", &data[used]) == 1) {
    if (++used == capacity) {
      capacity *= 2;
      data = realloc(data, capacity*sizeof(int32_t));
    }
  }

  uint64_t dims[1] = {used};
  header(out, 1, " i32", dims);
  fwrite(data, sizeof(int32_t), used, out);
}

void sequenceDouble(FILE *in, FILE *out) {
  int used = 0, capacity = 100;
  double *data = malloc(capacity*sizeof(double));
  while (fscanf(in, "%lf", &data[used]) == 1) {
    if (++used == capacity) {
      capacity *= 2;
      data = realloc(data, capacity*sizeof(double));
    }
  }

  uint64_t dims[1] = {used};
  header(out, 1, " f64", dims);
  fwrite(data, sizeof(double), used, out);
}

void sequenceDoublePair(FILE *in, FILE *out) {
  int used = 0, capacity = 100;
  double *data = malloc(capacity*sizeof(double));
  while (fscanf(in, "%lf %lf", &data[used], &data[used+1]) == 2) {
    if ((used+=2) > capacity/2) {
      capacity *= 2;
      data = realloc(data, capacity*sizeof(double));
    }
  }

  uint64_t dims[2] = {used/2, 2};
  header(out, 2, " f64", dims);
  fwrite(data, sizeof(double), used, out);
}

void sequenceIntPair(FILE *in, FILE *out) {
  int used = 0, capacity = 100;
  int32_t *data = malloc(capacity*sizeof(int32_t));
  while (fscanf(in, "%d %d", &data[used], &data[used+1]) == 2) {
    if ((used+=2) > capacity/2) {
      capacity *= 2;
      data = realloc(data, capacity*sizeof(int32_t));
    }
  }

  uint64_t dims[2] = {used/2, 2};
  header(out, 2, " i32", dims);
  fwrite(data, sizeof(int32_t), used, out);
}

void pbbs_triangles(FILE *in, FILE *out) {
  // Assuming 3D triangles.
  int n, m;
  int ret = fscanf(in, "%d\n%d\n", &n, &m);
  assert(ret == 2);

  uint64_t dims[2];
  dims[0] = n;
  dims[1] = 3;
  header(out, 2, " f32", dims);
  for (int i = 0; i < n; i++) {
    conv_float(in, out);
    conv_float(in, out);
    conv_float(in, out);
  }

  dims[0] = m;
  header(out, 2, " i32", dims);
  for (int i = 0; i < m; i++) {
    conv_i32(in, out);
    conv_i32(in, out);
    conv_i32(in, out);
  }
}

void pbbs_sequencePoint2d(FILE *in, FILE *out) {
  int used = 0, capacity = 100;
  double *data = malloc(capacity*sizeof(double));
  while (fscanf(in, "%lf", &data[used]) == 1) {
    if (++used == capacity) {
      capacity *= 2;
      data = realloc(data, capacity*sizeof(double));
    }
  }

  assert(used % 2 == 0);
  int n = used / 2;
  uint64_t dims[2] = {n, 2};
  header(out, 2, " f64", dims);
  fwrite(data, sizeof(double), used, out);
}

void pbbs_sequencePoint3d(FILE *in, FILE *out) {
  int used = 0, capacity = 100;
  double *data = malloc(capacity*sizeof(double));
  while (fscanf(in, "%lf", &data[used]) == 1) {
    if (++used == capacity) {
      capacity *= 2;
      data = realloc(data, capacity*sizeof(double));
    }
  }

  assert(used % 3 == 0);
  int n = used / 3;
  uint64_t dims[3] = {n, 3};
  header(out, 3, " f64", dims);
  fwrite(data, sizeof(double), used, out);
}

void AdjacencyGraph(FILE *in, FILE *out) {
  int n, m;
  int ret = fscanf(in, "%d\n%d\n", &n, &m);
  assert(ret == 2);

  uint64_t dims[1];
  dims[0] = n;
  header(out, 1, " i32", dims);
  for (int i = 0; i < n; i++) {
    conv_i32(in, out);
  }

  dims[0] = m;
  header(out, 1, " i32", dims);
  for (int i = 0; i < m; i++) {
    conv_i32(in, out);
  }
}

void EdgeArray(FILE *in, FILE *out) {
  sequenceIntPair(in, out);
}

void WeightedEdgeArray(FILE *in, FILE *out) {
  int used = 0, capacity = 100;
  int32_t *dataEdges = malloc(capacity*sizeof(int32_t));
  double *dataWeights = malloc(capacity*sizeof(double));
  while (fscanf(in, "%d %d %lf", &dataEdges[used], &dataEdges[used+1], &dataWeights[used/2]) == 3) {
    if ((used+=2) > capacity/2) {
      capacity *= 2;
      dataEdges = realloc(dataEdges, capacity*sizeof(int32_t));
      dataWeights = realloc(dataWeights, capacity*sizeof(double));
    }
  }

  uint64_t dimsEdges[2] = {used/2, 2};
  header(out, 2, " i32", dimsEdges);
  fwrite(dataEdges, sizeof(int32_t), used, out);
  uint64_t dimsWeights[1] = {used/2};
  header(out, 1, " f64", dimsWeights);
  fwrite(dataWeights, sizeof(double), used/2, out);
}

int main(int argc, char** argv) {
  char* line = NULL;
  size_t n;

  if (argc != 1) {
    fprintf(stderr, "%s takes no options.\n", argv[0]);
    exit(1);
  }

  if (isatty(fileno(stdin))) {
    fprintf(stderr, "stdin is a tty - you probably want to redirect from a file instead.\n");
  }

  if (isatty(fileno(stdout))) {
    fprintf(stderr, "stdout is a tty - redirect to a file instead.\n");
    exit(1);
  }

  getline(&line, &n, stdin);

  if (strcmp(line, "sequenceInt\n") == 0) {
    sequenceInt(stdin, stdout);
  } else if (strcmp(line, "sequenceDouble\n") == 0) {
    sequenceDouble(stdin, stdout);
  } else if (strcmp(line, "sequenceDoublePair\n") == 0) {
    sequenceDoublePair(stdin, stdout);
  } else if (strcmp(line, "sequenceIntPair\n") == 0) {
    sequenceIntPair(stdin, stdout);
  } else if (strcmp(line, "pbbs_triangles\n") == 0) {
    pbbs_triangles(stdin, stdout);
  } else if (strcmp(line, "pbbs_sequencePoint2d\n") == 0) {
    pbbs_sequencePoint2d(stdin, stdout);
  } else if (strcmp(line, "pbbs_sequencePoint3d\n") == 0) {
    pbbs_sequencePoint3d(stdin, stdout);
  } else if (strcmp(line, "AdjacencyGraph\n") == 0) {
    AdjacencyGraph(stdin, stdout);
  } else if (strcmp(line, "EdgeArray\n") == 0) {
    EdgeArray(stdin, stdout);
  } else if (strcmp(line, "WeightedEdgeArray\n") == 0) {
    WeightedEdgeArray(stdin, stdout);
  } else {
    fprintf(stderr, "Unknown file type: %s\n", line);
    exit(1);
  }

  free(line);
}

/*
 * Program to transform the binary-formatted Parboil data sets to
 * Futhark data format.  Lifted from Parboil, including the endianness
 * restriction(!).
 *
 * This program was not written to be nice to use.
 */

#include <endian.h>
#include <stdlib.h>
#include <malloc.h>
#include <stdio.h>
#include <inttypes.h>
#include <string.h>

#if __BYTE_ORDER != __LITTLE_ENDIAN
# error "File I/O is not implemented for this system: wrong endianness."
#endif

void inputData(char* fName, int* _numK, int* _numX,
               float** kx, float** ky, float** kz,
               float** x, float** y, float** z,
               float** phiR, float** phiI) {
  int numK, numX;
  FILE* fid = fopen(fName, "r");

  if (fid == NULL)
    {
      fprintf(stderr, "Cannot open input file\n");
      exit(-1);
    }
  fread (&numK, sizeof (int), 1, fid);
  *_numK = numK;
  fread (&numX, sizeof (int), 1, fid);
  *_numX = numX;
  *kx = (float *) memalign(16, numK * sizeof (float));
  fread (*kx, sizeof (float), numK, fid);
  *ky = (float *) memalign(16, numK * sizeof (float));
  fread (*ky, sizeof (float), numK, fid);
  *kz = (float *) memalign(16, numK * sizeof (float));
  fread (*kz, sizeof (float), numK, fid);
  *x = (float *) memalign(16, numX * sizeof (float));
  fread (*x, sizeof (float), numX, fid);
  *y = (float *) memalign(16, numX * sizeof (float));
  fread (*y, sizeof (float), numX, fid);
  *z = (float *) memalign(16, numX * sizeof (float));
  fread (*z, sizeof (float), numX, fid);
  *phiR = (float *) memalign(16, numK * sizeof (float));
  fread (*phiR, sizeof (float), numK, fid);
  *phiI = (float *) memalign(16, numK * sizeof (float));
  fread (*phiI, sizeof (float), numK, fid);
  fclose (fid);
}

void outputData(char* fName, uint32_t* numX, float** outR, float** outI) {
  FILE* fid = fopen(fName, "r");

  if (fid == NULL)
    {
      fprintf(stderr, "Cannot open input file\n");
      exit(-1);
    }
  fread (numX, sizeof (int), 1, fid);
  *outR = (float *) memalign(16, *numX * sizeof (float));
  fread (*outR, sizeof (float), *numX, fid);
  *outI = (float *) memalign(16, *numX * sizeof (float));
  fread (*outI, sizeof (float), *numX, fid);
  fclose (fid);
}

void print_array(const char *name, float* data, int num) {
  printf("-- %s\n[", name);
  for (int i = 0; i < num; i++) {
    printf("%ff", data[i]);
    if (i != num - 1) {
      printf(", ");
    }
  }
  printf("]\n");
}

int main(int argc, char** argv) {

  if (strcmp(argv[1], "in") == 0) {
    int numX, numK;		/* Number of X and K values */
    float *kx, *ky, *kz;		/* K trajectory (3D vectors) */
    float *x, *y, *z;		/* X coordinates (3D vectors) */
    float *phiR, *phiI;		/* Phi values (complex) */

    inputData(argv[2],
              &numK, &numX,
              &kx, &ky, &kz,
              &x, &y, &z,
              &phiR, &phiI);

    print_array("kx", kx, numK);
    print_array("ky", ky, numK);
    print_array("kz", kz, numK);
    print_array("x", x, numX);
    print_array("y", y, numX);
    print_array("z", z, numX);
    print_array("phiR", phiR, numK);
    print_array("phiI", phiI, numK);
  } else {
    uint32_t numX;
    float *outR, *outI;

    outputData(argv[2], &numX, &outR, &outI);
    print_array("outR", outR, numX);
    print_array("outI", outI, numX);
  }
}

// Convert an XSBench-generated binary data file to something Futhark
// can consume.  A bit fragile, since the file XSBench generates
// includes a raw dump of a C struct.

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

// Grid types
#define UNIONIZED 0
#define NUCLIDE 1
#define HASH 2

typedef struct{
	double energy;
	double total_xs;
	double elastic_xs;
	double absorbtion_xs;
	double fission_xs;
	double nu_fission_xs;
} NuclideGridPoint;

typedef struct{
	int * num_nucs;                     // Length = length_num_nucs;
	double * concs;                     // Length = length_concs
	int * mats;                         // Length = length_mats
	double * unionized_energy_array;    // Length = length_unionized_energy_array
	int * index_grid;                   // Length = length_index_grid
	NuclideGridPoint * nuclide_grid;    // Length = length_nuclide_grid
	int length_num_nucs;
	int length_concs;
	int length_mats;
	int length_unionized_energy_array;
	long length_index_grid;
	int length_nuclide_grid;
	int max_num_nucs;
	double * p_energy_samples;
	int length_p_energy_samples;
	int * mat_samples;
	int length_mat_samples;
} SimulationData;

void write_i64(FILE *fp, int64_t data) {
  putc('b', fp);
  putc(2, fp);
  putc(0, fp);
  fputs(" i64", fp);
  fwrite(&data, sizeof(data), 1, fp);
}


void write_i32_1d(FILE *fp, int32_t *data, int64_t n) {
  putc('b', fp);
  putc(2, fp);
  putc(1, fp);
  fputs(" i32", fp);
  fwrite(&n, sizeof(n), 1, fp);
  fwrite(data, sizeof(*data), n, fp);
}

void write_i32_2d(FILE *fp, int32_t *data, int64_t n, int64_t m) {
  putc('b', fp);
  putc(2, fp);
  putc(2, fp);
  fputs(" i32", fp);
  fwrite(&n, sizeof(n), 1, fp);
  fwrite(&m, sizeof(m), 1, fp);
  fwrite(data, sizeof(*data), n*m, fp);
}

void write_f64_1d(FILE *fp, double *data, int64_t n) {
  putc('b', fp);
  putc(2, fp);
  putc(1, fp);
  fputs(" f64", fp);
  fwrite(&n, sizeof(n), 1, fp);
  fwrite(data, sizeof(*data), n, fp);
}

void write_f64_2d(FILE *fp, double *data, int64_t n, int64_t m) {
  putc('b', fp);
  putc(2, fp);
  putc(2, fp);
  fputs(" f64", fp);
  fwrite(&n, sizeof(n), 1, fp);
  fwrite(&m, sizeof(m), 1, fp);
  fwrite(data, sizeof(*data), n*m, fp);
}

void write_f64_3d(FILE *fp, double *data, int64_t n, int64_t m, int64_t k) {
  putc('b', fp);
  putc(2, fp);
  putc(3, fp);
  fputs(" f64", fp);
  fwrite(&n, sizeof(n), 1, fp);
  fwrite(&m, sizeof(m), 1, fp);
  fwrite(&k, sizeof(k), 1, fp);
  fwrite(data, sizeof(*data), n*m*k, fp);
}

SimulationData binary_read(FILE* fp)
{
	SimulationData SD;

	// Read SimulationData Object. Include pointers, even though we won't be using them.
	fread(&SD, sizeof(SimulationData), 1, fp);

	// Allocate space for arrays on heap
	SD.num_nucs = (int *) malloc(SD.length_num_nucs * sizeof(int));
	SD.concs = (double *) malloc(SD.length_concs * sizeof(double));
	SD.mats = (int *) malloc(SD.length_mats * sizeof(int));
	SD.nuclide_grid = (NuclideGridPoint *) malloc(SD.length_nuclide_grid * sizeof(NuclideGridPoint));
	SD.index_grid = (int *) malloc( SD.length_index_grid * sizeof(int));
	SD.unionized_energy_array = (double *) malloc( SD.length_unionized_energy_array * sizeof(double));

	// Read heap arrays into SimulationData Object
	fread(SD.num_nucs,       sizeof(int), SD.length_num_nucs, fp);
	fread(SD.concs,          sizeof(double), SD.length_concs, fp);
	fread(SD.mats,           sizeof(int), SD.length_mats, fp);
	fread(SD.nuclide_grid,   sizeof(NuclideGridPoint), SD.length_nuclide_grid, fp); 
	fread(SD.index_grid, sizeof(int), SD.length_index_grid, fp);
	fread(SD.unionized_energy_array, sizeof(double), SD.length_unionized_energy_array, fp);

	fclose(fp);

	return SD;
}

int main(int argc, char** argv) {
  int64_t n_isotopes = 355;
  int64_t n_gridpoints = 11303;
  int64_t grid_type = UNIONIZED;
  int64_t hash_bins = 10000;
  int64_t lookups = 17000000;
  const char *HM = "small";

  if (strcmp(HM, "small") == 0) {
    n_isotopes = 68;
  } else {
    fprintf(stderr, "Unknown problem size: %s\n", HM);
  }

  assert(sizeof(NuclideGridPoint) == 6*sizeof(double));

  FILE *in = stdin, *out = stdout;
  SimulationData sd = binary_read(in);

  write_i64(out, n_isotopes);
  write_i64(out, n_gridpoints);
  write_i64(out, grid_type);
  write_i64(out, hash_bins);
  write_i64(out, lookups);

  write_i32_1d(out, sd.num_nucs, sd.length_num_nucs);
  write_f64_2d(out, sd.concs, sd.length_num_nucs, sd.max_num_nucs);
  write_i32_2d(out, sd.mats, sd.length_num_nucs, sd.max_num_nucs);
  write_f64_3d(out, (double*)sd.nuclide_grid, n_isotopes, n_gridpoints, 6);
  write_i32_2d(out, sd.index_grid, sd.length_unionized_energy_array, n_isotopes);
  write_f64_1d(out, sd.unionized_energy_array, sd.length_unionized_energy_array);
}

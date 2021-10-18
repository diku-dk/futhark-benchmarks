// Convert an RSBench-generated binary data file to something Futhark
// can consume.  A bit fragile, since the file RSBench generates
// includes a raw dump of a C struct.

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

// typedefs
typedef enum __hm{SMALL, LARGE, XL, XXL} HM_size;

#define HISTORY_BASED 1
#define EVENT_BASED 2

typedef struct{
	double r;
	double i;
} RSComplex;

typedef struct{
	int nthreads;
	int n_nuclides;
	int lookups;
	HM_size HM;
	int avg_n_poles;
	int avg_n_windows;
	int numL;
	int doppler;
	int particles;
	int simulation_method;
        int binary_mode;
	int kernel_id;
} Input;

typedef struct{
	RSComplex MP_EA;
	RSComplex MP_RT;
	RSComplex MP_RA;
	RSComplex MP_RF;
	short int l_value;
} Pole;

typedef struct{
	double T;
	double A;
	double F;
	int start;
	int end;
} Window;

typedef struct{
	int * n_poles;
	unsigned long length_n_poles;
	int * n_windows;
	unsigned long length_n_windows;
	Pole * poles;
	unsigned long length_poles;
	Window * windows;
	unsigned long length_windows;
	double * pseudo_K0RS;
	unsigned long length_pseudo_K0RS;
	int * num_nucs;
	unsigned long length_num_nucs;
	int * mats;
	unsigned long length_mats;
	double * concs;
	unsigned long length_concs;
	int max_num_nucs;
	int max_num_poles;
	int max_num_windows;
	double * p_energy_samples;
	unsigned long length_p_energy_samples;
	int * mat_samples;
	unsigned long length_mat_samples;
} SimulationData;

SimulationData binary_read( Input in ) {
  SimulationData SD;

  char * fname = "RS_data.dat";

  FILE * fp = stdin;
  assert(fp != NULL);

  // Read SimulationData Object. Include pointers, even though we won't be using them.
  fread(&SD, sizeof(SimulationData), 1, fp);

  // Allocate space for arrays on heap
  SD.n_poles =          malloc(SD.length_n_poles * sizeof(int));
  SD.n_windows =        malloc(SD.length_n_windows * sizeof(int));
  SD.poles =            malloc(SD.length_poles*sizeof(Pole));
  SD.windows =          malloc(SD.length_windows*sizeof(Window));
  SD.pseudo_K0RS =      malloc(SD.length_pseudo_K0RS*sizeof(double));
  SD.num_nucs =         malloc(SD.length_num_nucs*sizeof(int));
  SD.mats =             malloc(SD.length_mats*sizeof(int));
  SD.concs =            malloc(SD.length_concs*sizeof(double));
  SD.mat_samples =      malloc(SD.length_mat_samples*sizeof(int));

  // Read heap arrays into SimulationData Object
  assert(fread(SD.n_poles,          sizeof(int),    SD.length_n_poles, fp)==SD.length_n_poles);
  assert(fread(SD.n_windows,        sizeof(int),    SD.length_n_windows, fp)==SD.length_n_windows);
  assert(fread(SD.poles,            sizeof(Pole),   SD.length_poles, fp)==SD.length_poles);
  assert(fread(SD.windows,          sizeof(Window), SD.length_windows, fp)==SD.length_windows);
  assert(fread(SD.pseudo_K0RS,      sizeof(double), SD.length_pseudo_K0RS, fp)==SD.length_pseudo_K0RS);
  assert(fread(SD.num_nucs,         sizeof(int),    SD.length_num_nucs, fp)==SD.length_num_nucs);
  assert(fread(SD.mats,             sizeof(int),    SD.length_mats, fp)==SD.length_mats);
  assert(fread(SD.concs,            sizeof(double), SD.length_concs, fp)==SD.length_concs);

  fclose(fp);

  return SD;
}

void write_i32(FILE *fp, int32_t data) {
  putc('b', fp);
  putc(2, fp);
  putc(0, fp);
  fputs(" i32", fp);
  fwrite(&data, sizeof(data), 1, fp);
}

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

void write_i32_3d(FILE *fp, int32_t *data, int64_t n, int64_t m, int64_t k) {
  putc('b', fp);
  putc(2, fp);
  putc(3, fp);
  fputs(" i32", fp);
  fwrite(&n, sizeof(n), 1, fp);
  fwrite(&m, sizeof(m), 1, fp);
  fwrite(&k, sizeof(k), 1, fp);
  fwrite(data, sizeof(*data), n*m*k, fp);
}

void write_i16_2d(FILE *fp, int16_t *data, int64_t n, int64_t m) {
  putc('b', fp);
  putc(2, fp);
  putc(2, fp);
  fputs(" i16", fp);
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

void write_f64_4d(FILE *fp, double *data, int64_t n, int64_t m, int64_t k, int64_t l) {
  putc('b', fp);
  putc(2, fp);
  putc(4, fp);
  fputs(" f64", fp);
  fwrite(&n, sizeof(n), 1, fp);
  fwrite(&m, sizeof(m), 1, fp);
  fwrite(&k, sizeof(k), 1, fp);
  fwrite(&l, sizeof(l), 1, fp);
  fwrite(data, sizeof(*data), n*m*k*l, fp);
}


int main(int argc, char** argv) {
  assert(argc == 2);

  Input input;

  input.simulation_method = EVENT_BASED;
  input.particles = 300000;
  input.HM = LARGE;
  input.avg_n_poles = 1000;
  input.avg_n_windows = 100;
  input.numL = 4;
  input.doppler = 1;
  input.kernel_id = 0;
  input.lookups = 34 * 300000;

  const char *HM = argv[1];

  if (strcmp(HM, "small") == 0) {
    input.HM = SMALL;
    input.n_nuclides = 68;
  } else if (strcmp(HM, "large") == 0) {
    input.HM = LARGE;
    input.n_nuclides = 355;
  } else{
    fprintf(stderr, "Unknown problem size: %s\n", HM);
    return 1;
  }

  SimulationData sd = binary_read(input);
  FILE *out = stdout;

  write_i64(out, input.lookups);
  write_i32(out, input.doppler);
  write_i32_1d(out, sd.n_windows, sd.length_n_windows);

  uint16_t *poles_ls = malloc(sd.length_poles * sizeof(uint16_t));
  double *poles_cs = malloc(sd.length_poles * 4 * 2 * sizeof(double));

  for (int i = 0; i < sd.length_poles; i++) {
    poles_ls[i] = sd.poles[i].l_value;
    double *p = &poles_cs[i*4*2];
    p[0] = sd.poles[i].MP_EA.r;
    p[1] = sd.poles[i].MP_EA.i;
    p[2] = sd.poles[i].MP_RT.r;
    p[3] = sd.poles[i].MP_RT.i;
    p[4] = sd.poles[i].MP_RA.r;
    p[5] = sd.poles[i].MP_RA.i;
    p[6] = sd.poles[i].MP_RF.r;
    p[7] = sd.poles[i].MP_RF.i;
  }

  write_i16_2d(out, poles_ls, input.n_nuclides, sd.max_num_poles);
  write_f64_4d(out, poles_cs, input.n_nuclides, sd.max_num_poles, 4, 2);

  double *windows_f64s = malloc(sd.length_windows * 3 * sizeof(double));
  int32_t *windows_i32s = malloc(sd.length_windows * 2 * sizeof(int32_t));

  for (int i = 0; i < sd.length_windows; i++) {
    windows_f64s[i*3+0] = sd.windows[i].T;
    windows_f64s[i*3+1] = sd.windows[i].A;
    windows_f64s[i*3+2] = sd.windows[i].F;
    windows_i32s[i*2+0] = sd.windows[i].start;
    windows_i32s[i*2+1] = sd.windows[i].end;
  }

  write_f64_3d(out, windows_f64s, input.n_nuclides, sd.max_num_windows, 3);
  write_i32_3d(out, windows_i32s, input.n_nuclides, sd.max_num_windows, 2);

  write_f64_2d(out, sd.pseudo_K0RS, input.n_nuclides, input.numL);
  write_i32_1d(out, sd.num_nucs, sd.length_num_nucs);
  write_i32_2d(out, sd.mats, sd.length_num_nucs, sd.max_num_nucs);
  write_f64_2d(out, sd.concs, sd.length_num_nucs, sd.max_num_nucs);
}

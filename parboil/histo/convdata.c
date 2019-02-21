// Read a Parboil-formatted input data file on standard input and
// produce a Futhark-formatted output data file on standard output.

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

int main() {
  int result = 0;

  int32_t img_width, img_height;
  int32_t histo_width, histo_height;

  result += fread(&img_width,    sizeof(int32_t), 1, stdin);
  result += fread(&img_height,   sizeof(int32_t), 1, stdin);
  result += fread(&histo_width,  sizeof(int32_t), 1, stdin);
  result += fread(&histo_height, sizeof(int32_t), 1, stdin);

  if (result != 4){
    fprintf(stderr, "Error reading input and output dimensions from file\n");
    return -1;
  }

  int32_t* img = (int32_t*) malloc(img_width*img_height*sizeof(int32_t));

  result = fread(img, sizeof(int32_t), img_width*img_height, stdin);

  printf("b%c%c i32", 2, 0);
  fwrite(&histo_width, sizeof(int32_t), 1, stdout);
  printf("b%c%c i32", 2, 0);
  fwrite(&histo_height, sizeof(int32_t), 1, stdout);
  printf("b%c%c i32", 2, 2);
  int64_t img_width_64 = img_width, img_height_64 = img_height;
  fwrite(&img_width_64, sizeof(int64_t), 1, stdout);
  fwrite(&img_height_64, sizeof(int64_t), 1, stdout);
  fwrite(img, sizeof(int32_t), img_width*img_height, stdout);
}

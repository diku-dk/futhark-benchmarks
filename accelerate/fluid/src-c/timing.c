#include <stdlib.h>
#include "timing.h"

void timing_start(timing_t* timing) {
  gettimeofday(&(timing->time_start), NULL);
}

void timing_end(timing_t* t) {
  gettimeofday(&(t->time_end), NULL);

  unsigned int resolution = 1000000;
  long diff = (t->time_end.tv_usec + resolution * t->time_end.tv_sec
               - (t->time_start.tv_usec + resolution * t->time_start.tv_sec));
  t->time_diff.tv_sec = diff / resolution;
  t->time_diff.tv_usec = diff % resolution;
  t->usecs = t->time_diff.tv_sec * resolution + t->time_diff.tv_usec;
}

#ifndef FLUID_TIMER_H
#define FLUID_TIMER_H

#include <sys/time.h>

typedef struct {
  struct timeval time_start;
  struct timeval time_end;
  struct timeval time_diff;
  unsigned long usecs;
} timing_t;

void timing_start(timing_t* timing);

void timing_end(timing_t* t);

#endif

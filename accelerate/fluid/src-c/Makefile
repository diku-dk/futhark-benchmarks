.PHONY: clean

CC=gcc
CFLAGS=-O3 -Wall -Wextra -pedantic -Werror
GLFLAGS=-lglut -lGL -lGLU

all: fluid-visualize-densities fluid-benchmark fluid-demo

fluid-visualize-densities: fluid-visualize-densities.c solver.o
	$(CC) $(CFLAGS) -o fluid-visualize-densities fluid-visualize-densities.c solver.o

fluid-benchmark: fluid-benchmark.c solver.o timing.o
	$(CC) $(CFLAGS) -o fluid-benchmark fluid-benchmark.c solver.o timing.o

fluid-demo: fluid-demo.c solver.o
	$(CC) $(CFLAGS) $(GLFLAGS) -o fluid-demo fluid-demo.c solver.o

solver.o: solver.c solver.h
	$(CC) $(CFLAGS) -c solver.c

timing.o: timing.c timing.h
	$(CC) $(CFLAGS) -c timing.c

clean:
	rm -f fluid-visualize-densities fluid-benchmark fluid-demo solver.o timing.o

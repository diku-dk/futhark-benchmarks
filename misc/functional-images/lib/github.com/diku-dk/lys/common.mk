PROGNAME?=lys

all: $(PROGNAME)

NOWARN_CFLAGS=-std=c99 -O
CFLAGS?=$(NOWARN_CFLAGS) -Wall -Wextra -pedantic

OS=$(shell uname -s)
ifeq ($(OS),Darwin)
LDFLAGS?=-framework OpenCL -lm -lSDL2
else
LDFLAGS?=-lOpenCL -lm -lSDL2
endif

$(PROGNAME): $(PROGNAME)_wrapper.o lib/github.com/diku-dk/lys/liblys.c
	gcc lib/github.com/diku-dk/lys/liblys.c -I. -DPROGHEADER='"$(PROGNAME)_wrapper.h"' $(PROGNAME)_wrapper.o -o $@ $(LDFLAGS)

lib: futhark.pkg
	futhark pkg sync

# We do not want warnings and such for the generated code.
$(PROGNAME)_wrapper.o: $(PROGNAME)_wrapper.c
	gcc -o $@ -c $< $(NOWARN_CFLAGS)

%.c: %.fut lib
	futhark opencl --library $<

%_wrapper.fut: lib/github.com/diku-dk/lys/genlys.fut $(PROGNAME).fut
	cat $< | sed 's/"lys"/"$(PROGNAME)"/' > $@

run: $(PROGNAME)
	./$(PROGNAME)

clean:
	rm -f $(PROGNAME) $(PROGNAME).c $(PROGNAME).h $(PROGNAME)_wrapper.* *.o

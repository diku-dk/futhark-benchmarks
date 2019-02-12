PROGNAME?=lys

all: $(PROGNAME)

NOWARN_CFLAGS=-std=c99 -O
CFLAGS?=$(NOWARN_CFLAGS) -Wall -Wextra -pedantic

OS=$(shell uname -s)
ifeq ($(OS),Darwin)
OPENCLFLAGS?=-framework OpenCL
else
OPENCLFLAGS?=-lOpenCL
endif
LDFLAGS?=$(OPENCLFLAGS) -lm -lSDL2 -lSDL2_ttf

$(PROGNAME): $(PROGNAME)_wrapper.o lib/github.com/diku-dk/lys/liblys.c
	gcc lib/github.com/diku-dk/lys/liblys.c -I. -DPROGHEADER='"$(PROGNAME)_wrapper.h"' $(PROGNAME)_wrapper.o -o $@ $(CFLAGS) $(LDFLAGS)

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

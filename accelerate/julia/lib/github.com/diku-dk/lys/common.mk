PROGNAME?=lys
LYS_BACKEND?=opencl
PROG_FUT_DEPS=$(shell futhark imports $(PROGNAME).fut)

all: $(PROGNAME)

PKG_CFLAGS=$(shell pkg-config --cflags sdl2) $(shell pkg-config --cflags SDL2_ttf)
NOWARN_CFLAGS=-std=c11 -O
CFLAGS?=$(NOWARN_CFLAGS) $(PKG_CFLAGS) -Wall -Wextra -pedantic -DLYS_BACKEND_$(LYS_BACKEND)
BASE_LDFLAGS=-lm -lSDL2 -lSDL2_ttf

OS=$(shell uname -s)
ifeq ($(OS),Darwin)
OPENCL_LDFLAGS?=-framework OpenCL
else
OPENCL_LDFLAGS?=-lOpenCL
endif

ifeq ($(LYS_BACKEND),opencl)
LDFLAGS?=$(OPENCL_LDFLAGS) $(BASE_LDFLAGS)
else ifeq ($(LYS_BACKEND),cuda)
LDFLAGS?=$(BASE_LDFLAGS) -lcuda -lnvrtc
else ifeq ($(LYS_BACKEND),c)
LDFLAGS?=$(BASE_LDFLAGS)
else
$(error Unknown LYS_BACKEND: $(LYS_BACKEND).  Must be 'opencl', 'cuda', or 'c')
endif

ifeq ($(shell test futhark.pkg -nt lib; echo $$?),0)
$(PROGNAME):
	futhark pkg sync
	@make # The sync might have resulted in a new Makefile.
else
$(PROGNAME): $(PROGNAME)_wrapper.o $(PROGNAME)_printf.h lib/github.com/diku-dk/lys/liblys.c lib/github.com/diku-dk/lys/liblys.h
	gcc lib/github.com/diku-dk/lys/liblys.c -I. -DPROGHEADER='"$(PROGNAME)_wrapper.h"' -DPRINTFHEADER='"$(PROGNAME)_printf.h"' $(PROGNAME)_wrapper.o -o $@ $(CFLAGS) $(LDFLAGS)
endif

$(PROGNAME)_printf.h: $(PROGNAME)_wrapper.c
	python3 lib/github.com/diku-dk/lys/gen_printf.py $@ $<

# We do not want warnings and such for the generated code.
$(PROGNAME)_wrapper.o: $(PROGNAME)_wrapper.c
	gcc -o $@ -c $< $(NOWARN_CFLAGS)

%.c: %.fut
	futhark $(LYS_BACKEND) --library $<

%_wrapper.fut: lib/github.com/diku-dk/lys/genlys.fut $(PROG_FUT_DEPS)
	cat $< | sed 's/"lys"/"$(PROGNAME)"/' > $@

run: $(PROGNAME)
	./$(PROGNAME)

clean:
	rm -f $(PROGNAME) $(PROGNAME).c $(PROGNAME).h $(PROGNAME)_wrapper.* $(PROGNAME)_printf.h *.o

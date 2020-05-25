.PHONY: all run clean

PROGNAME?=lys

all: $(PROGNAME)

LYS_TTF=1

ifeq ($(shell test futhark.pkg -nt lib; echo $$?),0)
$(PROGNAME):
	futhark pkg sync
	@make # The sync might have resulted in a new Makefile.
else
include lib/github.com/diku-dk/lys/setup_flags.mk
$(PROGNAME): $(PROGNAME)_wrapper.o $(PROGNAME)_printf.h lib/github.com/diku-dk/lys/liblys.c lib/github.com/diku-dk/lys/liblys.h lib/github.com/diku-dk/lys/context_setup.c lib/github.com/diku-dk/lys/context_setup.h lib/github.com/diku-dk/lys/main.c
	gcc lib/github.com/diku-dk/lys/liblys.c lib/github.com/diku-dk/lys/context_setup.c lib/github.com/diku-dk/lys/main.c -I. -DPROGHEADER='"$(PROGNAME)_wrapper.h"' -DPRINTFHEADER='"$(PROGNAME)_printf.h"' $(PROGNAME)_wrapper.o -o $@ $(CFLAGS) $(LDFLAGS)
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

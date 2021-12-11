.PHONY: all run clean

PROGNAME?=lys

all: $(PROGNAME)

LYS_TTF=1

SELF_DIR := $(dir $(lastword $(MAKEFILE_LIST)))

ifeq ($(shell test futhark.pkg -nt lib; echo $$?),0)
$(PROGNAME):
	futhark pkg sync
	@make # The sync might have resulted in a new Makefile.
else
include $(SELF_DIR)/setup_flags.mk
$(PROGNAME): $(PROGNAME)_wrapper.o $(PROGNAME)_printf.h font_data.h $(SELF_DIR)/liblys.c $(SELF_DIR)/liblys.h $(SELF_DIR)/context_setup.c $(SELF_DIR)/context_setup.h $(SELF_DIR)/main.c
	gcc $(SELF_DIR)/liblys.c $(SELF_DIR)/context_setup.c $(SELF_DIR)/main.c -I. -DPROGHEADER='"$(PROGNAME)_wrapper.h"' -DPRINTFHEADER='"$(PROGNAME)_printf.h"' $(PROGNAME)_wrapper.o -o $@ $(CFLAGS) $(LDFLAGS)
endif

$(PROGNAME)_printf.h: $(PROGNAME)_wrapper.c
	python3 $(SELF_DIR)/gen_printf.py $(SELF_DIR) $@ $<

font_data.h: $(SELF_DIR)/Inconsolata-Regular.ttf
	echo 'unsigned char font_data[] = {' > $@
	xxd -i - < $< >> $@
	echo '};' >> $@

# We do not want warnings and such for the generated code.
$(PROGNAME)_wrapper.o: $(PROGNAME)_wrapper.c
	gcc -o $@ -c $< $(NOWARN_CFLAGS)

%.c: %.fut
	futhark $(LYS_BACKEND) --library $<

%_wrapper.fut: $(SELF_DIR)/genlys.fut $(PROG_FUT_DEPS)
	cat $< | sed 's/"lys"/"$(PROGNAME)"/' > $@

run: $(PROGNAME)
	./$(PROGNAME)

clean:
	rm -f $(PROGNAME) $(PROGNAME).c $(PROGNAME).h $(PROGNAME)_wrapper.* $(PROGNAME)_printf.h *.o font_data.h

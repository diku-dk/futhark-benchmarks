.PHONY: run clean

all: nbody.py

nbody.py: nbody.fut
	futhark pyopencl --library nbody.fut

nbody.c: nbody.fut
	futhark opencl --library nbody.fut

_nbody.so: nbody.c
	build_futhark_ffi nbody

clean:
	rm -f nbody.py *.pyc *.so *.o *.c

run: nbody.py
	@python nbody-gui.py

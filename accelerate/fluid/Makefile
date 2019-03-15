.PHONY: clean

all: fluid.py

run: all
	python fluid-gui.py 300

fluid.py: fluid.fut
	futhark pyopencl --library fluid.fut

fluid.c: fluid.fut
	futhark opencl --library fluid.fut

_fluid.so: fluid.c
	build_futhark_ffi fluid

clean:
	rm -f fluid.py fluid.pyc *.o *.so *.c

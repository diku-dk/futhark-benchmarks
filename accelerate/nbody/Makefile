.PHONY: run clean

all: nbody.py

nbody.py: nbody.fut
	futhark-pyopencl --library nbody.fut

clean:
	rm -f nbody.py *.pyc

run: nbody.py
	@python nbody-gui.py

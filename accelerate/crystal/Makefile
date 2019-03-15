.PHONY: run clean

all: crystal.py

crystal.py: crystal.fut
	futhark pyopencl --library crystal.fut

clean:
	rm -f crystal.py

run: crystal.py
	@python crystal-gui.py

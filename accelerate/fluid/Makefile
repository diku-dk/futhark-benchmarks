.PHONY: clean

all: fluid.py

run: all
	python fluid-gui.py 300

fluid.py: fluid.fut
	futhark-pyopencl --library fluid.fut -o fluid.py

clean:
	rm -f fluid.py fluid.pyc

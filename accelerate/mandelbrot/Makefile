.PHONY: run clean

all: mandelbrot.py

mandelbrot.py: mandelbrot.fut
	futhark-pyopencl --library mandelbrot.fut

clean:
	rm mandelbrot.py

run: mandelbrot.py
	@python mandelbrot-gui.py

.PHONY: run clean

all: mandelbrot32.py mandelbrot64.py

mandelbrot32.py: mandelbrot32.fut
	futhark-pyopencl --library mandelbrot32.fut

mandelbrot64.py: mandelbrot64.fut
	futhark-pyopencl --library mandelbrot64.fut

clean:
	rm -f mandelbrot32.py mandelbrot64.py

run: mandelbrot32.py mandelbrot64.py
	@python mandelbrot-gui.py

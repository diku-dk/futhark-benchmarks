all: images.py

run: images.py
	python gui.py

images.py: images.fut
	futhark-pyopencl --library $<

clean:
	rm -f images.py *.pyc

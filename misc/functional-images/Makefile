run: images.py
	python gui.py

images.py: images.fut
	futhark-pyopencl --library $<

all: canny.py

clean:
	rm canny.py

canny.py: canny.fut
	futhark pyopencl --library canny.fut

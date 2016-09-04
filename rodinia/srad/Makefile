srad.py: srad.fut
	futhark-pyopencl --library srad.fut

run: srad.py
	python3 srad-gui.py

clean:
	rm -f *.pyc srad.py

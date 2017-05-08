all: trace.py

run: trace.py
	python ray-gui.py

trace.py: *.fut
	futhark-pyopencl --library trace.fut

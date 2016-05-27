.PHONY: run clean

all: life.py quadlife.py quadlife_alt.py

life.py: life.fut
	futhark-pyopencl --library life.fut

quadlife.py: quadlife.fut
	futhark-pyopencl --library quadlife.fut

quadlife_alt.py: quadlife_alt.fut
	futhark-pyopencl --library quadlife_alt.fut

clean:
	rm -f life.py quadlife.py quadlife_alt.py *.pyc

run: life.py quadlife.py quadlife_alt.py
	@python life-gui.py

.PHONY: run clean

all: life.py

life.py: $(shell echo *.fut)
	futhark-pyopencl --library life.fut
clean:
	rm -f life.py *.pyc

run: life.py quadlife.py quadlife_alt.py
	@python life-gui.py

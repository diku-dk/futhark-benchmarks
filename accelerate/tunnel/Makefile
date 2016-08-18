.PHONY: run clean

all: tunnel.py

tunnel.py: tunnel.fut
	futhark-pyopencl --library tunnel.fut

clean:
	rm -f tunnel.py

run: tunnel.py
	@python tunnel-gui.py

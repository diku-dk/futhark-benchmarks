.PHONY: run clean

all: life.py

life.py: life.fut
	futhark-pyopencl --module life.fut

clean:
	rm life.py

run: life.py
	@python life-visualise.py

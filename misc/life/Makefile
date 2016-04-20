.PHONY: run clean

all: life.py quadlife.py

life.py: life.fut
	futhark-pyopencl --module life.fut

quadlife.py: quadlife.fut
	futhark-pyopencl --module quadlife.fut

clean:
	rm life.py

run: life.py quadlife.py
	@python life-visualise.py

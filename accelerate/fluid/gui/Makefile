.PHONY: clean

all: fluid.py

fluid.py: ../src-futhark/fluid_visualize_densities_one_frame_rgb.fut
	futhark-pyopencl --module ../src-futhark/fluid_visualize_densities_one_frame_rgb.fut -o fluid.py

clean:
	rm -f fluid.py fluid.pyc

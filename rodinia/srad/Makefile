srad: srad_core.py
	@touch srad

srad_core.py: srad_core.fut
	futhark-pyopencl --real-as-single --module srad_core.fut

clean:
	rm -f srad_core.py

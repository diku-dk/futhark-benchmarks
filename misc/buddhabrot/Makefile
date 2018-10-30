buddhabrot.py: buddhabrot.fut
	futhark-pyopencl --library buddhabrot.fut

run: buddhabrot.py
	./buddhabrot-gui.py

clean:
	rm -f *.pyc buddhabrot.py

SRAD
====

A de-blurring application that operates on grayscale images.  When run
with `make run`, it will open an image picker.  Select an image file
and it will be loaded.  Requires Python 3, Tk, SciPy, and Pygame.  The
latter two are the `python3-tk` and `python3-scipy` packages in
Debian.  Pygame does not have a package in Debian as of this writing
(it will presumably be `python3-pygame`), but you can install it with
`pip`.  First install `pip` (`python3-pip`), then `pip3 install
pygame`.

#!/usr/bin/env python

import mandelbrot
import png
import numpy

def test_write():
    width=1000
    height=1000
    image=mandelbrot.main(height, width, 50, -2.5, -1.5, 1.5, 1.5)
    name='mandelbrot.png'
    f = open(name, 'wb')
    w = png.Writer(height, width, greyscale=False, alpha=False, bitdepth=8)
    w.write(f, numpy.reshape(image, (height, width*3)))


if __name__ == '__main__':
    test_write()

#!/usr/bin/env python

import mandelbrot
import png
import numpy

def test_write():
    width=1200
    height=900
    image=mandelbrot.main(width, height, 256, -2.5, -1.5, 1.5, 1.5)
    name='mandelbrot2.png'
    f = open(name, 'wb')
    w = png.Writer(width, height, greyscale=False, alpha=False, bitdepth=8)
    w.write(f, numpy.reshape(image, (height, width*3)))


if __name__ == '__main__':
    test_write()

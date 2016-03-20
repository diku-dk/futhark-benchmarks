#!/usr/bin/env python

import sys
import numpy
import png

numpy.set_printoptions(threshold=numpy.nan)

def toImage(filename, fut_image):
    height=fut_image.shape[0]
    width=fut_image.shape[1]
    image=numpy.empty((height,width,3))
    image[:,:,0] = (fut_image & 0xFF0000) >> 16
    image[:,:,1] = (fut_image & 0xFF00) >> 8
    image[:,:,2] = (fut_image & 0xFF)

    w = png.Writer(width, height, greyscale=False, alpha=False, bitdepth=8)
    with open(filename, 'wb') as f:
        w.write(f, numpy.reshape(image, (height, width*3)))

res=numpy.array(eval(''.join(sys.stdin.readlines()).replace('i32', '')), dtype=numpy.int32)
toImage('out.png', res)

#!/usr/bin/env python

import canny
import numpy
from scipy import misc
import argparse

numpy.set_printoptions(threshold=numpy.nan)

def main(infile):
    c = canny.canny()
    img3 = misc.imread(infile, mode='RGBA')
    (height, width, channels) = img3.shape
    img = numpy.zeros((height,width))
    img[:,:] += img3[:,:,0] << 24
    img[:,:] += img3[:,:,1] << 16
    img[:,:] += img3[:,:,2] << 8
    img[:,:] += img3[:,:,3] << 0
    edges = c.main(50, 100, img)
    print edges

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Apply canny edge detection to PNG file.')
    parser.add_argument('filename', metavar='INFILE', type=str,
                        help='The PNG file in which to detect edges')
    args = parser.parse_args()
    main(args.filename)

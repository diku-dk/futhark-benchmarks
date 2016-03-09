#!/usr/bin/env python

import mandelbrot
import png
import numpy
import time
import argparse

def main(filename, width, height, limit, minx, miny, maxx, maxy):

    start=time.time()
    fut_image=mandelbrot.main(width, height, limit, minx, miny, maxx, maxy)
    end=time.time()
    print('Computed fractal in %.2fs' % (end-start,))

    # Futhark gives us an array of 32-bit integers encoding the color,
    # but the PNG writer expects each colour channel to be separate.
    start=time.time()
    image=numpy.empty((height,width,3))
    image[:,:,0] = (fut_image & 0xFF0000) >> 16
    image[:,:,1] = (fut_image & 0xFF00) >> 8
    image[:,:,2] = (fut_image & 0xFF)
    end=time.time()
    print('Prepared Numpy array in in %.2fs' % (end-start,))

    start=time.time()
    w = png.Writer(width, height, greyscale=False, alpha=False, bitdepth=8)
    with open(filename, 'wb') as f:
        w.write(f, numpy.reshape(image, (height, width*3)))
    end=time.time()
    print('Encoded %s in %.2fs' % (filename, end-start))

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Generate a Mandelbrot fractal.')
    parser.add_argument('--width', metavar='N', type=int, default=800,
                        help='The width of the generated image in pixels')
    parser.add_argument('--height', metavar='N', type=int, default=600,
                        help='The height of the generated image in pixels')
    parser.add_argument('--limit', metavar='N', type=int, default=255,
                        help='The number of iterations to test for convergence')
    parser.add_argument('--min-x', metavar='X', type=float, default=-2.23,
                        help='The x-component of the lower left corner of the region')
    parser.add_argument('--min-y', metavar='Y', type=float, default=-1.15,
                        help='The y-component of the lower left corner of the region')
    parser.add_argument('--max-x', metavar='X', type=float, default=0.83,
                        help='The x-component of the upper right corner of the region')
    parser.add_argument('--max-y', metavar='Y', type=float, default=1.15,
                        help='The y-component of the upper right corner of the region')
    parser.add_argument('filename', metavar='OUTFILE', type=str,
                        help='The filename that the resulting PNG image is written to.')
    args = parser.parse_args()
    main(args.filename, args.width, args.height,
         args.limit, args.min_x, args.min_y, args.max_x, args.max_y)

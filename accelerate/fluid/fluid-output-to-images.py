#!/usr/bin/env python3

import sys
import os
import png
import numpy as np
import math


def trunc(x):
    if x > 255:
        return 255
    elif x < 0:
        return 0
    else:
        return x

def n_digits(n_elems):
    return int(math.log(n_elems, 10)) + 1

def main(args):
    try:
        out_dir = args[0]
    except IndexError:
        print('error: output directory must be given as the first argument',
              file=sys.stderr)
        return 1
    
    try:
        os.makedirs(out_dir)
    except IOError:
        print('error: output directory already exists',
              file=sys.stderr)
        return 1
    
    trunc_np = np.vectorize(trunc)
    images = trunc_np(np.array(eval(sys.stdin.read().replace('i32', ''))))

    for image, i in zip(images, range(len(images))):
        filename = os.path.join(
            out_dir,
            ('fluid_{:0' + str(n_digits(len(images))) + 'd}.png').format(i))
        with open(filename, 'wb') as f:
            w = png.Writer(image.shape[1], image.shape[0], greyscale=True, bitdepth=8)
            w.write(f, image)

    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))

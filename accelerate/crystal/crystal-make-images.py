#!/usr/bin/env python3

import sys
import os
import subprocess
import math
import png
import numpy as np


def n_digits(n_elems):
    return int(math.log(n_elems, 10)) + 1

def main(args):
    try:
        field_size, scale, degree, n_steps, time_delta, output_directory = args
    except Exception:
        print('''\
usage: ./crystal-make-image.py <field size> <scale> <degree> <n steps> <time delta> <output directory>

Example:
  ./crystal-make-image.py 200 30.0 5 50 0.5 test/\
''',
              file=sys.stderr)
        return 1

    try:
        os.makedirs(output_directory)
    except IOError:
        print('error: output directory already exists',
              file=sys.stderr)
        return 1
    
    inp = ''.join(s + '\n' for s in
                  (field_size, scale, degree, n_steps, time_delta)).encode()
    outp = subprocess.Popen(['./crystal'], stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE).communicate(inp)[0]
    outp = outp.replace(b'i32', b'').decode()
    images = eval(outp)
    size = int(field_size)
    for image, i in zip(images, range(len(images))):
        image = np.array(image).reshape((size, size * 3))

        filename = os.path.join(
            output_directory,
            ('frame_{:0' + str(max(8, n_digits(len(images)))) + 'd}.png').format(i))

        with open(filename, 'wb') as f:
            w = png.Writer(size, size, bitdepth=8)
            w.write(f, image)

    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))

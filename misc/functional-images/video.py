#!/usr/bin/env python

from images import images

import numpy as np
import time
import sys
import argparse
import imageio

images = images()
(width, height, _) = images.test_image_render(0.0)

parser = argparse.ArgumentParser(description='Functional images')
parser.add_argument('--seconds', metavar='INT', type=int,
                    help='Number of seconds of video to produce.', default=10)
parser.add_argument('--framerate', metavar='INT', type=int,
                    help='Framerate of video.', default=60)
parser.add_argument('output', metavar='FILE', type=str,
                    help='Framerate of video.')
args = parser.parse_args()

with imageio.get_writer(args.output, mode='I') as writer:
    for f in range(args.seconds*args.framerate):
        t = f*(1.0/args.framerate)
        (_, _, frame) = images.test_image_render(np.float32(t))
        frame = np.transpose(frame.get())
        image = np.ndarray(frame.shape + (3,), np.uint8)
        image[:,:,0] = frame[:,:] & 0xFF
        image[:,:,1] = (frame[:,:] >> 8) & 0xFF
        image[:,:,2] = (frame[:,:] >> 16) & 0xFF
        writer.append_data(image)
        if (f%args.framerate == 0):
            print('Rendered {} seconds.'.format(f/args.framerate))

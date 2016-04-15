#!/usr/bin/env python

import life
import quadlife
import numpy
import pygame
import time
import pyopencl as cl

STEPS_PER_RUN=3
width=800
height=600
size=(width,height)
frame_every=1.0/30.0

# TODO: add command line parameter for picking between life and quadlife.

def main():
#    l=life.life()
    l=quadlife.quadlife()
    screen = pygame.display.set_mode(size)
    initworld = numpy.random.choice([True, False], size=size)
    world, history = l.init(initworld)
    surface = pygame.Surface(size)
    screen.blit(surface, (0, 0))

    last_frame = time.time()
    while True:
        world, history = l.steps(world, history, STEPS_PER_RUN)
        if time.time() > last_frame + frame_every:
            frame = l.render_frame(history)
            pygame.surfarray.blit_array(surface, frame)
            screen.blit(surface, (0, 0))
            pygame.display.flip()
            last_frame = time.time()

if __name__ == '__main__':
    main()

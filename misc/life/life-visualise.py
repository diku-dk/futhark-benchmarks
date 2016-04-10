#!/usr/bin/env python

import life as futlife
import numpy
import pygame
import time
import pyopencl as cl

STEPS_PER_RUN=3
width=800
height=600
size=(width,height)
frame_every=1.0/30.0
def main():
    life=futlife.life()
    screen = pygame.display.set_mode(size)
    initworld = numpy.random.choice([True, False], size=size)
    world, history = life.init(initworld)
    surface = pygame.Surface(size)
    screen.blit(surface, (0, 0))

    last_frame = time.time()
    while True:
        world, history = life.steps(world, history, STEPS_PER_RUN)

        if time.time() > last_frame + frame_every:
            frame = life.render_frame(history)
            pygame.surfarray.blit_array(surface, frame)
            screen.blit(surface, (0, 0))
            pygame.display.flip()
            last_frame = time.time()

if __name__ == '__main__':
    main()

#!/usr/bin/env python

import life
import numpy
import pygame
import time

STEPS_PER_RUN=20
i = 0
outdir='out'
width=800
height=600
size=(width,height)
frame_every=1.0/30.0
def main():
    screen = pygame.display.set_mode(size)
    initworld = numpy.random.choice([True, False], size=size)
    world, history = life.init(initworld)
    channel_frame=numpy.empty((width, height, 3), dtype=numpy.uint8)
    surface = pygame.Surface(size)
    screen.blit(surface, (0, 0))

    last_frame = time.time()
    while True:
        world, history = life.steps(world, history, STEPS_PER_RUN)

        if time.time() > last_frame + frame_every:
            frame = life.render_frame(history)
            channel_frame[:,:,0] = (frame & 0xFF0000) >> 16
            channel_frame[:,:,1] = (frame & 0xFF00) >> 8
            channel_frame[:,:,2] = (frame & 0xFF)
            pygame.surfarray.blit_array(surface, channel_frame)
            screen.blit(surface, (0, 0))
            pygame.display.flip()
            last_frame = time.time()

if __name__ == '__main__':
    main()

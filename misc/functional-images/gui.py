#!/usr/bin/env python

from images import images

import numpy as np
import pygame
import time
import sys

images = images(interactive=True)
(width, height, _) = images.test_image_render(0.0)

size=(width, height)
pygame.init()
pygame.display.set_caption('Functional Images')

screen = pygame.display.set_mode(size)
surface = pygame.Surface(size, depth=32)
font = pygame.font.Font(None, 36)

def showText(what, where):
    text = font.render(what, 1, (255, 255, 255))
    screen.blit(text, where)

t0 = time.time()

def render():
    futhark_start = time.time()
    (_, _, frame) = images.test_image_render(np.float32(futhark_start-t0))
    frame = frame.get()
    futhark_end = time.time()
    pygame.surfarray.blit_array(surface, frame)
    screen.blit(surface, (0, 0))
    speedmsg = "Futhark calls took %.2fms" % ((futhark_end-futhark_start)*1000)
    showText(speedmsg, (10, 10))
    pygame.display.flip()

pygame.key.set_repeat(500, 50)

while True:
    render()
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()

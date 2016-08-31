#!/usr/bin/env python

from crystal import crystal

import numpy
import pygame
import time
import sys

field_size=1000
scale=30.0
degree=5
size=(field_size,field_size)

crystal_frame = crystal().render_frame

pygame.init()
pygame.display.set_caption('Crystal')
screen = pygame.display.set_mode(size)
surface = pygame.Surface(size)
font = pygame.font.Font(None, 36)

def showText(what, where):
    text = font.render(what, 1, (0, 0, 0))
    screen.blit(text, where)

def render(t):
    frame_start = time.time()
    frame = crystal_frame(field_size, scale, degree, t).get()
    frame_end = time.time()
    pygame.surfarray.blit_array(surface, frame)
    screen.blit(surface, (0, 0))

    speedmessage = "Futhark call took %.2fms" % ((frame_end-frame_start)*1000)
    showText(speedmessage, (10, 10))

    degreemessage = "Degree: %d (left/right to change)" % degree
    showText(degreemessage, (10, 30))

    scalemessage = "Scale: %.2f (up/down to change)" % scale
    showText(scalemessage, (10, 50))


    pygame.display.flip()

prev_t = time.time()
i = 0
pygame.key.set_repeat(500, 50)

while True:
    render(time.time()-prev_t)
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_RIGHT:
                degree += 1
            if event.key == pygame.K_LEFT:
                degree -= 1
            if event.key == pygame.K_UP:
                scale += 0.5
            if event.key == pygame.K_DOWN:
                scale -= 0.5

#!/usr/bin/env python

from images import images

import numpy as np
import pygame
import time
import sys

images = images(interactive=True)
width = 800

screen_width = width
screen_height = width
size=(screen_width, screen_height)
aspect_ratio = float(screen_width)/float(screen_height)
pygame.init()
pygame.display.set_caption('Functional Images')

screen = pygame.display.set_mode(size)
surface = pygame.Surface(size, depth=32)
font = pygame.font.Font(None, 36)

def showText(what, where):
    text = font.render(what, 1, (255, 255, 255))
    screen.blit(text, where)

t0 = time.time()
(xcentre, ycentre) = (0, 0)

def render():
    futhark_start = time.time()
    frame = images.test_pan_image(screen_width, screen_height,
                                  width, width,
                                  xcentre, ycentre,
                                  np.float32(futhark_start-t0))
    frame = frame.get()
    futhark_end = time.time()
    pygame.surfarray.blit_array(surface, frame)
    screen.blit(surface, (0, 0))
    speedmsg = "Futhark calls took %.2fms" % ((futhark_end-futhark_start)*1000)
    showText(speedmsg, (10, 10))
    pygame.display.flip()

pygame.key.set_repeat(500, 50)

def moveXPixels(pixels):
    global xcentre
    unit_per_pixel = width/screen_width
    xcentre += unit_per_pixel * pixels

def moveYPixels(pixels):
    global ycentre
    height = width*(1/aspect_ratio)
    unit_per_pixel = height/screen_height
    ycentre += unit_per_pixel * pixels

while True:
    render()
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_RIGHT:
                centre_x += width*0.01
            if event.key == pygame.K_LEFT:
                centre_x -= width*0.01
            if event.key == pygame.K_UP:
                centre_y -= width*0.01
            if event.key == pygame.K_DOWN:
                centre_y += width*0.01
            if event.key == pygame.K_w:
                width *= 0.99
            if event.key == pygame.K_s:
                width *= 1.01

        elif event.type == pygame.MOUSEBUTTONDOWN:
            # Handle scroll wheel.
            if event.button == 4:
                width *= 0.99
            elif event.button == 5:
                width *= 1.01

        elif event.type == pygame.MOUSEMOTION:
            if event.buttons[0] == 1:
                (xd,yd) = event.rel
                moveXPixels(-xd)
                moveYPixels(-yd)

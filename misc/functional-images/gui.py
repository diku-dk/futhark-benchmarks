#!/usr/bin/env python

from images import images

import numpy as np
import pygame
import time
import sys

images = images(interactive=True)
width = 10.0

screen_width = 800
screen_height = 800
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

tcur = 0.0
tlast = time.time()
(xcentre, ycentre) = (0, 0)
(xuser, yuser) = (0.5, 0.5)
paused = False
direction = 1

images = { '1': images.mandelbrot_greyscale,
           '2': images.julia_greyscale,
           '3': images.mandelbrot_colour,
           '4': images.julia_colour,
           '5': images.figure_7_15 }
image = images['1']

def render():
    global tlast, tcur
    futhark_start = time.time()
    tdelta = futhark_start - tlast if not paused else 0
    tcur += tdelta * direction
    tlast = futhark_start
    frame = image(xuser, yuser,
                  np.float32(tcur),
                  screen_width, screen_height,
                  width, width,
                  xcentre, ycentre)
    frame = frame.get()
    futhark_end = time.time()
    pygame.surfarray.blit_array(surface, frame)
    screen.blit(surface, (0, 0))
    speedmsg = "Futhark calls took %.2fms" % ((futhark_end-futhark_start)*1000)
    showText(speedmsg, (10, 10))
    posmsg = "Centre position: (%.4f, %.4f)" % (xcentre, ycentre)
    showText(posmsg, (10, 30))
    usermsg = "Right-click position: (%.4f, %.4f)" % (xuser, yuser)
    showText(usermsg, (10, 50))
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

def rightClick(pos):
    global xuser, yuser
    xuser = event.pos[0]/float(screen_width)
    yuser = event.pos[1]/float(screen_height)

while True:
    render()
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_RIGHT:
                xcentre += width*0.01
            if event.key == pygame.K_LEFT:
                xcentre -= width*0.01
            if event.key == pygame.K_UP:
                ycentre -= width*0.01
            if event.key == pygame.K_DOWN:
                ycentre += width*0.01
            if event.key == pygame.K_w:
                width *= 0.99
            if event.key == pygame.K_s:
                width *= 1.01
            if event.unicode in images:
                image = images[event.unicode]
            if event.unicode == '-':
                direction *= -1
            if event.key == pygame.K_SPACE:
                paused = not paused

        elif event.type == pygame.MOUSEBUTTONDOWN:
            # Handle scroll wheel.
            if event.button == 4:
                width *= 0.99
            elif event.button == 5:
                width *= 1.01
            elif event.button == 3:
                rightClick(event.pos)

        elif event.type == pygame.MOUSEMOTION:
            if event.buttons[0] == 1:
                (xd,yd) = event.rel
                moveXPixels(-xd)
                moveYPixels(-yd)
            # Handle right click
            if event.buttons[2] == 1:
                rightClick(event.pos)

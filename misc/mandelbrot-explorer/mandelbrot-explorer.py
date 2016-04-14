#!/usr/bin/env python

import mandelbrot
import numpy
import pygame
import time
import sys

width=1200
height=800
limit=255
size=(width,height)
frame_every=1.0/30.0

m = mandelbrot.mandelbrot()

def make_mandelbrot(minx, miny, maxx, maxy):
    return m.main(width, height, limit, minx, miny, maxx, maxy)

minx=-2.23
miny=-1.15
maxx=0.83
maxy=1.15

aspect_ratio = float(width)/float(height)
x_inc_ratio = 1.0 + 0.01 * aspect_ratio
x_dec_ratio = 1.0 - 0.01 * aspect_ratio
y_inc_ratio = 1.0 + 0.01
y_dec_ratio = 1.0 - 0.01

def zoomIn():
    global minx, maxx, miny, maxy
    x_dist = abs(maxx-minx)
    y_dist = abs(maxy-miny)
    minx += x_dist * 0.01
    maxx -= x_dist * 0.01
    miny += y_dist * 0.01
    maxy -= y_dist * 0.01

def zoomOut():
    global minx, maxx, miny, maxy
    x_dist = abs(maxx-minx)
    y_dist = abs(maxy-miny)
    minx -= x_dist * 0.01
    maxx += x_dist * 0.01
    miny -= y_dist * 0.01
    maxy += y_dist * 0.01

def moveLeft():
    global minx, maxx
    x_dist = abs(maxx-minx)
    minx -= x_dist * 0.01
    maxx -= x_dist * 0.01

def moveRight():
    global minx, maxx
    x_dist = abs(maxx-minx)
    minx += x_dist * 0.01
    maxx += x_dist * 0.01

def moveUp():
    global miny, maxy
    y_dist = abs(maxy-miny)
    miny -= y_dist * 0.01
    maxy -= y_dist * 0.01

def moveDown():
    global miny, maxy
    y_dist = abs(maxy-miny)
    miny += y_dist * 0.01
    maxy += y_dist * 0.01

pygame.init()
pygame.display.set_caption('Mandelbrot Explorer!')
screen = pygame.display.set_mode(size)
surface = pygame.Surface(size)
font = pygame.font.Font(None, 36)
pygame.key.set_repeat(1, 1)

def showText(what, where):
    text = font.render(what, 1, (255, 255, 255))
    screen.blit(text, where)

def render():
    start = time.time()
    frame = make_mandelbrot(minx, miny, maxx, maxy)
    end = time.time()
    pygame.surfarray.blit_array(surface, frame)
    screen.blit(surface, (0, 0))

    infomessage = "Region: (%f,%f) to (%f,%f)    Rendering limit: %d" % (minx, miny, maxx, maxy, limit)
    showText(infomessage, (0,0))

    speedmessage = "Futhark call took %2fms" % ((end-start)*1000)
    showText(speedmessage, (0, height-36))
    pygame.display.flip()
    end = time.time()

while True:
    render()
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        if event.type == pygame.KEYDOWN:
            if event.key == pygame.K_RIGHT:
                moveRight()
            if event.key == pygame.K_LEFT:
                moveLeft()
            if event.key == pygame.K_UP:
                moveUp()
            if event.key == pygame.K_DOWN:
                moveDown()
            if event.unicode == 'q':
                limit -= 1
            if event.unicode == 'w':
                limit += 1
            if event.unicode == '+':
                zoomIn()
            if event.unicode == '-':
                zoomOut()

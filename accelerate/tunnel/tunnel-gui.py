#!/usr/bin/env python

import tunnel

import numpy
import pygame
import time
import sys

width=1200
height=800
size=(width,height)

render_tunnel = tunnel.tunnel(interactive=True).main

pygame.init()
pygame.display.set_caption('Tunnel')
screen = pygame.display.set_mode(size)
font = pygame.font.Font(None, 36)

curtime=0

def showText(what, where):
    text = font.render(what, 1, (255, 255, 255))
    screen.blit(text, where)

def render():
    start = time.time()
    frame = render_tunnel(curtime, width, height).get()
    end = time.time()
    pygame.surfarray.blit_array(screen, frame)

    speedmessage = "Futhark call took %.2fms" % ((end-start)*1000)
    showText(speedmessage, (10, 10))

    pygame.display.flip()

while True:
    curtime += 0.005
    render()
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()

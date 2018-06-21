#!/usr/bin/env python

import trace

import numpy as np
import pygame
import time
import sys

width=1024
height=768
fov=100
bouncelimit=4
rtime=0.0
size=(width,height)
orig_eye = { 'point': {'x': 50, 'y': -100, 'z': -700},
             'vector': {'x': 0, 'y': 0, 'z': 1} }
raytracer = trace.trace(interactive=True)

pygame.init()
pygame.display.set_caption('Raytracer!')
screen = pygame.display.set_mode(size)
surface = pygame.Surface(size, depth=32)
font = pygame.font.Font(None, 36)
pygame.key.set_repeat(1, 0)

def showText(what, where):
    text = font.render(what, 1, (255, 255, 255))
    screen.blit(text, where)

tcur = 0.0
tlast = time.time()
paused = False

def render(eye):
    global tlast, tcur
    start = time.time()
    tdelta = start - tlast if not paused else 0
    tcur += tdelta
    tlast = start
    frame = raytracer.main(width,height,fov,
                           eye['point']['x'], eye['point']['y'], eye['point']['z'],
                           eye['vector']['x'], eye['vector']['y'], eye['vector']['z'],
                           bouncelimit,np.float32(tcur)).get()
    end = time.time()
    pygame.surfarray.blit_array(surface, frame)
    screen.blit(surface, (0, 0))

    speedmessage = "Futhark call took %.2fms (%d bounces)" % ((end-start)*1000, bouncelimit)
    showText(speedmessage, (10, 10))
    locmessage = ("Position: (%.2f, %.2f, %.2f) Orientation: (%.2f, %.2f, %.2f)" %
                  (eye['point']['x'], eye['point']['y'], eye['point']['z'],
                   eye['vector']['x'], eye['vector']['y'], eye['vector']['z']))
    showText(locmessage, (10, 40))

    pygame.display.flip()

eye = orig_eye
changed_bounce_limit = False
while True:
    render(eye)
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_SPACE:
                paused = not paused
            if event.key == pygame.K_RIGHT:
                eye['point']['x'] -= 1
            if event.key == pygame.K_LEFT:
                eye['point']['x'] += 1
            if event.key == pygame.K_PAGEDOWN:
                eye['point']['y'] -= 1
            if event.key == pygame.K_PAGEUP:
                eye['point']['y'] += 1
            if event.key == pygame.K_UP:
                eye['point']['z'] += 1
            if event.key == pygame.K_DOWN:
                eye['point']['z'] -= 1
            if event.key == pygame.K_HOME:
                eye = orig_eye
            if event.unicode == 'q' and not changed_bounce_limit:
                bouncelimit = max(bouncelimit-1,1)
                changed_bounce_limit = True
            if event.unicode == 'w' and not changed_bounce_limit:
                bouncelimit += 1
                changed_bounce_limit = True
        elif event.type == pygame.KEYUP:
            if event.key in [pygame.K_q, pygame.K_w]:
                changed_bounce_limit = False

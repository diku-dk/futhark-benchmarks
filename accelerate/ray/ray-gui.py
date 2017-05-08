#!/usr/bin/env python

import trace

import numpy
import pygame
import time
import sys

width=1024
height=768
fov=100
bouncelimit=4
rtime=0.0
size=(width,height)
orig_eye_pos = (50,-100,-700)

raytracer = trace.trace(interactive=True)

pygame.init()
pygame.display.set_caption('Raytracer!')
screen = pygame.display.set_mode(size)
surface = pygame.Surface(size)
font = pygame.font.Font(None, 36)
pygame.key.set_repeat(1, 0)

def showText(what, where):
    text = font.render(what, 1, (255, 255, 255))
    screen.blit(text, where)

def render(eye_pos):
    global rtime
    rtime += 0.01
    start = time.time()
    frame = raytracer.render(width,height,fov,
                             eye_pos[0], eye_pos[1], eye_pos[2],
                             bouncelimit,rtime).get()
    end = time.time()
    pygame.surfarray.blit_array(surface, frame)
    screen.blit(surface, (0, 0))

    speedmessage = "Futhark call took %.2fms (%d bounces)" % ((end-start)*1000, bouncelimit)
    showText(speedmessage, (10, 10))

    pygame.display.flip()

(eye_x,eye_y,eye_z) = orig_eye_pos
while True:
    render((eye_x,eye_y,eye_z))
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_RIGHT:
                eye_x -= 1
            if event.key == pygame.K_LEFT:
                eye_x += 1
            if event.key == pygame.K_UP:
                eye_y -= 1
            if event.key == pygame.K_DOWN:
                eye_y += 1
            if event.key == pygame.K_PAGEDOWN:
                eye_z -= 1
            if event.key == pygame.K_PAGEUP:
                eye_z += 1
            if event.key == pygame.K_HOME:
                (eye_x,eye_y,eye_z) = orig_eye_pos
            if event.unicode == 'q':
                bouncelimit = max(bouncelimit-1,1)
            if event.unicode == 'w':
                bouncelimit += 1


#!/usr/bin/env python

import trace

import numpy as np
import pygame
import time
import sys
import math

width=1024
height=768
fov=100
bouncelimit=4
rtime=0.0
size=(width,height)
orig_eye = { 'point': {'x': 50, 'y': -100, 'z': -700},
             'dir': {'a': math.pi/2, 'b': 0} }
raytracer = trace.trace(interactive=True)

pygame.init()
pygame.display.set_caption('Raytracer!')
screen = pygame.display.set_mode(size)
surface = pygame.Surface(size, depth=32)
font = pygame.font.Font(None, 36)
pygame.key.set_repeat(500, 50)

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
                           eye['dir']['a'], eye['dir']['b'],
                           bouncelimit,np.float32(tcur)).get()
    end = time.time()
    pygame.surfarray.blit_array(surface, frame)
    screen.blit(surface, (0, 0))

    speedmessage = "Futhark call took %.2fms (%d bounces)" % ((end-start)*1000, bouncelimit)
    showText(speedmessage, (10, 10))
    locmessage = ("Position: (%.2f, %.2f, %.2f) Orientation: (%.2f, %.2f)" %
                  (eye['point']['x'], eye['point']['y'], eye['point']['z'],
                   eye['dir']['a'], eye['dir']['b']))
    showText(locmessage, (10, 40))

    pygame.display.flip()

eye = orig_eye

def forwards(amount):
    global eye
    a = eye['dir']['a']
    b = eye['dir']['b']
    eye['point']['x'] += amount * math.cos(a) * math.cos(b)
    eye['point']['y'] += amount * math.sin(b)
    eye['point']['z'] += amount * math.sin(a) * math.cos(b)

def sideways(amount):
    global eye
    a = eye['dir']['a'] + math.pi/2
    eye['point']['x'] += amount * math.cos(a)
    eye['point']['z'] += amount * math.sin(a)

movspeed = 1000
rotspeed = math.pi
pygame.event.set_grab(True)
pygame.mouse.set_visible(False)
while True:
    delta = time.time()-tlast
    render(eye)
    delta_x, delta_y = pygame.mouse.get_rel()
    eye['dir']['a'] += float(delta_x)/width
    eye['dir']['b'] += float(delta_y)/height
    eye['dir']['a'] = eye['dir']['a'] % (math.pi*2)
    eye['dir']['b'] = min(max(eye['dir']['b'], -math.pi/2+0.001), math.pi/2-0.001)

    pressed = pygame.key.get_pressed()
    if pressed[pygame.K_a]:
        sideways(-movspeed*delta)
    elif pressed[pygame.K_d]:
        sideways(movspeed*delta)
    elif pressed[pygame.K_PAGEDOWN]:
        eye['point']['y'] -= 1
    elif pressed[pygame.K_PAGEUP]:
        eye['point']['y'] += 1
    elif pressed[pygame.K_w] or pressed[pygame.K_UP]:
        forwards(movspeed*delta)
    elif pressed[pygame.K_s] or pressed[pygame.K_DOWN]:
        forwards(-movspeed*delta)
    elif pressed[pygame.K_RIGHT]:
        eye['dir']['a'] = (eye['dir']['a'] + rotspeed*delta) % (math.pi*2)
    elif pressed[pygame.K_LEFT]:
        eye['dir']['a'] = (eye['dir']['a'] - rotspeed*delta) % (math.pi*2)

    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_SPACE:
                paused = not paused
            if event.key == pygame.K_ESCAPE:
                sys.exit()
            if event.key == pygame.K_HOME:
                eye = orig_eye
            if event.unicode == 'z':
                bouncelimit = max(bouncelimit-1,1)
            if event.unicode == 'x':
                bouncelimit += 1

#!/usr/bin/env python

import mandelbrot32
import mandelbrot64
import numpy
import pygame
import time
import sys

width=1024
height=768
limit=255
radius=4.0
size=(1024,768)
frame_every=1.0/30.0
startpos=(-2.23,-1.15,0.83,1.15)

presets = {
    '0': (-0.7,                   0.0,                             3.067,                  100,   16.0),
    '1': (0.20508818500545423,    0.9014915666351141   * 900/1440, 6.375321937544527e-6,   629,   256.0),
    '2': (0.4510757067879078,     0.6144133202705898   * 900/1440, 7.632248223018773e-5,   399,   4.0),
    '3': (0.3469337523117071,     0.6866350870407725   * 900/1440, 3.508380713647269e-5,   505,   1048576.0),
    '4': (-0.7902001921590814,    0.24910667566731381  * 900/1440, 5.071115028132377e-4,   1176,  3.4359738368e10),
    '5': (2.3127178455019423e-2, -1.301205470975472    * 900/1440, 3.6349313304610088e-9,  566,   4.0),
    '6': (2.3127176148480418e-2, -1.3012054707668765   * 900/1440, 2.71444790387451e-10,   604,   4.0),
    '7': (2.3127176156746785e-2, -1.301205470242045    * 900/1440, 4.49615119202067e-12,   2000,  4.0),
    '8': (0.2550376327692795,     8.962363618058007e-4 * 900/1440, 7.351698819132829e-5,   1412,  256.0),
    '9': (0.25498593633806477,    8.726424280526077e-4 * 900/1440, 1.6858526052251987e-10, 10492, 4.0)
}

precision=32
m32 = mandelbrot32.mandelbrot32(interactive=True)
m64 = None # Loaded on demand.
mandelbrot = m32

def make_mandelbrot(minx, miny, maxx, maxy):
    return mandelbrot.render_mandelbrot(width, height, limit, radius, minx, miny, maxx, maxy).get()

backend='Futhark'

minx, miny, maxx, maxy = startpos

aspect_ratio = float(width)/float(height)
x_inc_ratio = 1.0 + 0.01 * aspect_ratio
x_dec_ratio = 1.0 - 0.01 * aspect_ratio
y_inc_ratio = 1.0 + 0.01
y_dec_ratio = 1.0 - 0.01

def setPreset(preset):
    global minx,miny,maxx,maxy,limit,radius
    (preset_x, preset_y, preset_width, preset_limit, preset_radius) = presets[preset]
    minx=preset_x-preset_width/2
    miny=preset_y-(1/aspect_ratio)*preset_width/2
    maxx=preset_x+preset_width/2
    maxy=preset_y+(1/aspect_ratio)*preset_width/2
    limit=preset_limit
    radius=preset_radius

def switchPrecision():
    global m64, mandelbrot, precision
    if precision == 32:
        if m64 == None:
            m64 = mandelbrot64.mandelbrot64(device_pref=m32.device.name)
        precision = 64
        mandelbrot = m64
    else:
        precision = 32
        mandelbrot = m32

def resetPos():
    global minx, maxx, miny, maxy
    minx, miny, maxx, maxy = startpos

def zoomTo(pos, factor):
    global minx, maxx, miny, maxy
    pos_x, pos_y = pos
    rel_x = float(pos_x) / float(width)
    rel_y = float(pos_y) / float(height)
    x_span = maxx - minx
    y_span = maxy - miny
    x = minx + x_span * rel_x
    y = miny + y_span * rel_y

    minx = x - factor * x_span
    maxx = x + factor * x_span
    miny = y - factor * y_span
    maxy = y + factor * y_span

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

def moveXPixels(pixels):
    global minx, maxx
    unit_per_pixel = (maxx-minx)/width
    minx += unit_per_pixel * pixels
    maxx += unit_per_pixel * pixels

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

def moveYPixels(pixels):
    global miny, maxy
    unit_per_pixel = (maxy-miny)/height
    miny += unit_per_pixel * pixels
    maxy += unit_per_pixel * pixels

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

    infomessage = "Region: (%f,%f) to (%f,%f); bits: %d; iterations: %d; radius: %.2f" % (minx, miny, maxx, maxy, precision, limit, radius)
    showText(infomessage, (10,10))

    speedmessage = "%s call took %.2fms" % (backend, (end-start)*1000)
    showText(speedmessage, (10, 40))

    pygame.display.flip()

setPreset('0')

while True:
    render()
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()

        elif event.type == pygame.KEYDOWN:
            keydown=True
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
            if event.unicode == 'e':
                limit += 1
            if event.key == pygame.K_w:
                zoomIn()
            if event.key == pygame.K_s:
                zoomOut()
            if event.key == pygame.K_z:
                radius = radius * 0.99
            if event.key == pygame.K_c:
                radius = radius * 1.01
            if event.key == pygame.K_ESCAPE:
                sys.exit()

            if presets.get(event.unicode) != None:
                setPreset(event.unicode)

        elif event.type == pygame.KEYUP:
            if event.key == pygame.K_a:
                limit = int(limit * 0.8)
            if event.key == pygame.K_d:
                limit = int(limit * 1.2)
            if event.key == pygame.K_p:
                switchPrecision()

        elif event.type == pygame.MOUSEBUTTONDOWN:
            # Handle scroll wheel.
            if event.button == 4:
                zoomIn()
            elif event.button == 5:
                zoomOut()

        elif event.type == pygame.MOUSEMOTION:
            if event.buttons[0] == 1:
                (xd,yd) = event.rel
                moveXPixels(-xd)
                moveYPixels(-yd)

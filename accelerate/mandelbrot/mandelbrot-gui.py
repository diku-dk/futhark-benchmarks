#!/usr/bin/env python

import mandelbrot32
import mandelbrot64
import numpy
import pygame
import time
import sys

screenX=1024
screenY=768
limit=255
radius=4.0
size=(screenX,screenY)
aspect_ratio = float(screenX)/float(screenY)
frame_every=1.0/30.0

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

def make_mandelbrot():
    return mandelbrot.render_mandelbrot(screenX, screenY, xcentre, ycentre, width, limit, radius).get()

(xcentre,ycentre,width,limit,radius) = presets['0']

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

def zoomIn():
    global width
    width *= 0.99

def zoomOut():
    global width
    width *= 1.01

def moveLeft():
    global xcentre
    xcentre -= width*0.01

def moveRight():
    global xcentre
    xcentre += width*0.01

def moveXPixels(pixels):
    global xcentre
    unit_per_pixel = width/screenX
    xcentre += unit_per_pixel * pixels

def moveUp():
    global ycentre
    height = width*(1/aspect_ratio)
    ycentre -= height*0.01

def moveDown():
    global ycentre
    height = width*(1/aspect_ratio)
    ycentre += height*0.01

def moveYPixels(pixels):
    global ycentre
    height = width*(1/aspect_ratio)
    unit_per_pixel = height/screenY
    ycentre += unit_per_pixel * pixels

pygame.init()
pygame.display.set_caption('Mandelbrot Explorer!')
screen = pygame.display.set_mode(size)
surface = pygame.Surface(size, depth=32)
font = pygame.font.Font(None, 36)
pygame.key.set_repeat(1, 1)

def showText(what, where):
    text = font.render(what, 1, (255, 255, 255))
    screen.blit(text, where)

def render():
    start = time.time()
    frame = make_mandelbrot()
    end = time.time()
    pygame.surfarray.blit_array(surface, frame)
    screen.blit(surface, (0, 0))

    infomessage = "Centre: (%f,%f); width: %f; bits: %d; iterations: %d; radius: %.2f" % (xcentre, ycentre, width, precision, limit, radius)
    showText(infomessage, (10,10))

    speedmessage = "Futhark call took %.2fms" % ((end-start)*1000,)
    showText(speedmessage, (10, 40))

    pygame.display.flip()

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
                (xcentre,ycentre,width,limit,radius) = presets[event.unicode]

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

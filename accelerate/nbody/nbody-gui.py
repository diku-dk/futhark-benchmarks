#!/usr/bin/env python
#
# There is some joystick support, but it's hardwired for a USB NES
# controller I keep on my desk.

import nbody

import numpy
import pygame
import time
import sys

nb = nbody.nbody(interactive=True)

pygame.init()
pygame.display.set_caption('N-body')
screen = pygame.display.set_mode((800,600))
width = screen.get_width()
height = screen.get_height()
size = (width, height)

x_rotation=0.0
y_rotation=0.0
(x_ul, y_ul, x_br, y_br) = (-width/2, -height/2, width/2, height/2)
default_time_step = 1.0
steps_per_call = 1

render_nbody = nb.render
step_nbody = nb.main
N = 5000
epsilon=50.0
max_mass=1.0

def random_points():
    return (numpy.random.normal(size=N,scale=width/5,loc=0).astype('float32'),
            numpy.random.normal(size=N,scale=height/5,loc=0).astype('float32'),
            numpy.random.normal(size=N,scale=height/5,loc=0).astype('float32'),

            numpy.random.rand(N).astype('float32'),

            numpy.zeros(N).astype('float32'),
            numpy.zeros(N).astype('float32'),
            numpy.zeros(N).astype('float32'),

            numpy.zeros(N).astype('float32'),
            numpy.zeros(N).astype('float32'),
            numpy.zeros(N).astype('float32'))

def random_points_orbit():
    xs = numpy.random.normal(size=N,scale=width/10,loc=0).astype('float32')
    return (xs,
            numpy.random.normal(size=N,scale=height/200,loc=0).astype('float32') * (xs/100),
            numpy.random.normal(size=N,scale=height/200,loc=0).astype('float32') * (xs/10),

            numpy.random.rand(N).astype('float32'),

            numpy.zeros(N).astype('float32'),
            xs/width*10,
            numpy.zeros(N).astype('float32'),

            numpy.zeros(N).astype('float32'),
            numpy.zeros(N).astype('float32'),
            numpy.zeros(N).astype('float32'))

def random_points_donut():
    angles = numpy.random.rand(N).astype('float32') * numpy.pi * 2
    tangents = numpy.pi*2 - angles
    lengthscales = numpy.random.normal(size=N).astype('float32')
    lengths = lengthscales*min(width,height)/10 + min(width,height)/2
    return (lengths * numpy.sin(angles),
            lengths * numpy.cos(angles),
            numpy.random.rand(N).astype('float32') * width / 10,

            numpy.random.rand(N).astype('float32'),

            numpy.sin(angles-numpy.pi/2)*2*numpy.abs(lengthscales),
            numpy.cos(angles-numpy.pi/2)*2*numpy.abs(lengthscales),
            numpy.zeros(N).astype('float32'),

            numpy.zeros(N).astype('float32'),
            numpy.zeros(N).astype('float32'),
            numpy.zeros(N).astype('float32'))

def random_points_spiral():
    lengths = numpy.arange(N).astype('float32') / (N/(min(width,height)/2))
    angles = lengths / (3*numpy.pi)
    return (lengths * numpy.sin(angles),
            lengths * numpy.cos(angles),
            numpy.random.rand(N).astype('float32') * width / 10,

            numpy.random.rand(N).astype('float32'),

            numpy.zeros(N).astype('float32'),
            numpy.zeros(N).astype('float32'),
            numpy.zeros(N).astype('float32'),

            numpy.zeros(N).astype('float32'),
            numpy.zeros(N).astype('float32'),
            numpy.zeros(N).astype('float32'))


(xps,yps,zps,ms,xvs,yvs,zvs,xas,yas,zas) = random_points()

font = pygame.font.Font(None, 36)

curtime=0

def showText(what, where, color):
    text = font.render(what, 1, color)
    screen.blit(text, where)

def render(time_step,invert):
    global xps,yps,zps,ms,xvs,yvs,zvs,xas,yas,zas,angle
    start = time.time()
    (xps,yps,zps,ms,xvs,yvs,zvs,xas,yas,zas) = \
      step_nbody(steps_per_call, epsilon, time_step, xps,yps,zps,ms,xvs,yvs,zvs,xas,yas,zas)
    frame = render_nbody(width, height, x_ul, y_ul, x_br, y_br, xps, yps, zps, ms, x_rotation, y_rotation, max_mass, invert).get()
    end = time.time()
    futhark_time = (end-start)*1000
    start = time.time()
    pygame.surfarray.blit_array(screen, frame)
    end = time.time()
    blit_time = (end-start)*1000

    speedmessage = "Futhark call took %.2fms; blitting %.2fms (N=%d, timestep=%s)" % \
                   (futhark_time, blit_time, N,
                    "(paused)" if time_step == 0 else str(time_step))
    color = (0,0,0) if invert else (255, 255, 255)
    showText(speedmessage, (10, 10), color)

    pygame.display.flip()

def point(x,y,z):
    return nb.inverseRotatePoint(x, y, z, -x_rotation, -y_rotation)

def blob(pos):
    global N, xps,yps,zps,ms,xvs,yvs,zvs,xas,yas,zas
    x_dist = float(x_br-x_ul)
    y_dist = float(y_br-y_ul)
    x,y = pos
    x = x_ul + (float(x) / width) * x_dist
    y = y_ul + (float(y) / height) * y_dist
    (x,y,z) = point(x,y,0)
    blob_N = 100

    angles = numpy.random.rand(blob_N).astype('float32') * numpy.pi * 2
    z_angles = numpy.random.rand(blob_N).astype('float32') * numpy.pi * 2
    lengths = numpy.random.rand(blob_N).astype('float32') * min(width,height)/100

    xps = numpy.concatenate((xps.get(), x + lengths * numpy.cos(angles)))
    yps = numpy.concatenate((yps.get(), y + lengths * numpy.sin(angles)))
    zps = numpy.concatenate((zps.get(), z + lengths * numpy.sin(z_angles)))
    ms = numpy.concatenate((ms.get(), numpy.random.rand(blob_N).astype('float32')))
    xvs = numpy.concatenate((xvs.get(),  numpy.zeros(blob_N).astype('float32')))
    yvs = numpy.concatenate((yvs.get(), numpy.zeros(blob_N).astype('float32')))
    zvs = numpy.concatenate((zvs.get(), numpy.zeros(blob_N).astype('float32')))
    xas = numpy.concatenate((xas.get(), numpy.zeros(blob_N).astype('float32')))
    yas = numpy.concatenate((yas.get(), numpy.zeros(blob_N).astype('float32')))
    zas = numpy.concatenate((zas.get(), numpy.zeros(blob_N).astype('float32')))

    N += blob_N

def zoomOut():
    global x_br,x_ul,y_br,y_ul
    x_dist = float(x_br-x_ul)
    y_dist = float(y_br-y_ul)

    x_br += x_dist * 0.01
    x_ul -= x_dist * 0.01
    y_br += y_dist * 0.01
    y_ul -= y_dist * 0.01

def zoomIn():
    global x_br,x_ul,y_br,y_ul
    x_dist = float(x_br-x_ul)
    y_dist = float(y_br-y_ul)

    x_br -= x_dist * 0.01
    x_ul += x_dist * 0.01
    y_br -= y_dist * 0.01
    y_ul += y_dist * 0.01

def moveLeft(delta):
    global x_ul, x_br
    x_dist = abs(x_br-x_ul)
    x_ul -= x_dist * delta
    x_br -= x_dist * delta

def moveRight(delta):
    global x_ul, x_br
    x_dist = abs(x_br-x_ul)
    x_ul += x_dist * delta
    x_br += x_dist * delta

def moveUp(delta):
    global y_ul, y_br
    y_dist = abs(y_br-x_ul)
    y_ul -= y_dist * delta
    y_br -= y_dist * delta

def moveDown(delta):
    global y_ul, y_br
    y_dist = abs(y_br-x_ul)
    y_ul += y_dist * delta
    y_br += y_dist * delta

mass_active = False
def mouseMass(pos):
    global ms, xps, yps, zps

    x_dist = float(x_br-x_ul)
    y_dist = float(y_br-y_ul)
    x,y = pos
    x = x_ul + (float(x) / width) * x_dist
    y = y_ul + (float(y) / height) * y_dist
    (x,y,z) = point(x,y,0)

    if mass_active and pos:
        ms[0] = 10000
        xps[0] = x
        yps[0] = y
        zps[0] = z
        xvs[0] = 0
        yvs[0] = 0
        zvs[0] = 0
    else:
        ms[0] = 0.0001
        xps[0] = x_br
        yps[0] = y_br


pygame.key.set_repeat(1, 1)

# Joystick stuff.
pygame.joystick.init()

JOYSTICK_X_AXIS=0
JOYSTICK_Y_AXIS=1
JOYSTICK_ROTATE_BUTTON=1

joystick = None
joystick_count = pygame.joystick.get_count()
for i in range(joystick_count):
    joystick = pygame.joystick.Joystick(i)
    joystick.init()

time_step = default_time_step
time_then = time.time()

invert = False
while True:
    time_now = time.time()
    time_delta = time_now - time_then
    time_then = time_now

    rotating_x = 0.0
    rotating_y = 0.0

    render(time_step,invert)
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        elif event.type == pygame.KEYDOWN:
            repeats = 5 if pygame.key.get_mods() & pygame.KMOD_CTRL else 1
            for i in range(repeats):
                if pygame.key.get_mods() & pygame.KMOD_SHIFT:
                    if event.key == pygame.K_RIGHT:
                        y_rotation += 0.01
                    if event.key == pygame.K_LEFT:
                        y_rotation -= 0.01
                    if event.key == pygame.K_UP:
                        x_rotation += 0.01
                    if event.key == pygame.K_DOWN:
                        x_rotation -= 0.01
                else:
                    if event.key == pygame.K_RIGHT:
                        moveRight(time_delta)
                    if event.key == pygame.K_LEFT:
                        moveLeft(time_delta)
                    if event.key == pygame.K_UP:
                        moveUp(time_delta)
                    if event.key == pygame.K_DOWN:
                        moveDown(time_delta)
                if event.unicode == '+':
                    zoomIn()
                if event.unicode == '-':
                    zoomOut()
            if event.unicode == 'c':
                N = 1
                (xps,yps,zps,ms,xvs,yvs,zvs,xas,yas,zas) = random_points()
            if event.unicode == 'r':
                (xps,yps,zps,ms,xvs,yvs,zvs,xas,yas,zas) = random_points()
            if event.unicode == 'o':
                (xps,yps,zps,ms,xvs,yvs,zvs,xas,yas,zas) = random_points_orbit()
            if event.unicode == 'd':
                (xps,yps,zps,ms,xvs,yvs,zvs,xas,yas,zas) = random_points_donut()
            if event.unicode == 's':
                (xps,yps,zps,ms,xvs,yvs,zvs,xas,yas,zas) = random_points_spiral()
            if event.unicode == 'q':
                time_step -= 0.01
            if event.unicode == 'w':
                time_step += 0.01
        elif event.type == pygame.KEYUP:
            if event.key == pygame.K_SPACE:
                if time_step > 0:
                    default_time_step = time_step
                    time_step = 0
                else:
                    time_step = default_time_step
            if event.key == pygame.K_HOME:
                x_rotation=0.0
                y_rotation=0.0
                (x_ul, y_ul, x_br, y_br) = (-width/2, -height/2, width/2, height/2)
            if event.key == pygame.K_i:
                invert = not invert
        elif event.type == pygame.MOUSEBUTTONDOWN:
            if pygame.mouse.get_pressed()[0]:
                blob(pygame.mouse.get_pos())
            mass_active = pygame.mouse.get_pressed()[2] == 1
        elif event.type == pygame.MOUSEBUTTONUP:
            mass_active = pygame.mouse.get_pressed()[2] == 1

    if joystick:
        rotating_y = joystick.get_axis(JOYSTICK_X_AXIS)
        rotating_x = joystick.get_axis(JOYSTICK_Y_AXIS)

    x_rotation += rotating_x * time_delta
    y_rotation += rotating_y * time_delta

    mouseMass(pygame.mouse.get_pos())

#!/usr/bin/env python

import nbody

import numpy
import pygame
import time
import sys

width=1200
height=800
x_rotation=0.0
y_rotation=0.0
size=(width,height)
(x_ul, y_ul, x_br, y_br) = (-width/2, -height/2, width/2, height/2)
default_time_step = 0.1
steps_per_call = 1

nb = nbody.nbody()
render_nbody = nb.render
step_nbody = nb.main
N = 5000
epsilon=50.0

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
    xs = numpy.random.normal(size=N,scale=height/5,loc=0).astype('float32')
    return (xs,
            numpy.random.normal(size=N,scale=height/200,loc=0).astype('float32') * (xs/100),
            numpy.random.normal(size=N,scale=height/200,loc=0).astype('float32') * (xs/10),

            numpy.random.rand(N).astype('float32'),

            numpy.zeros(N).astype('float32'),
            numpy.sign(xs).astype('float32'),
            numpy.zeros(N).astype('float32'),

            numpy.zeros(N).astype('float32'),
            numpy.zeros(N).astype('float32'),
            numpy.zeros(N).astype('float32'))


(xps,yps,zps,ms,xvs,yvs,zvs,xas,yas,zas) = random_points()

pygame.init()
pygame.display.set_caption('N-body')
screen = pygame.display.set_mode(size)
surface = pygame.Surface(size)
font = pygame.font.Font(None, 36)

curtime=0

def showText(what, where):
    text = font.render(what, 1, (255, 255, 255))
    screen.blit(text, where)

def render(time_step):
    global xps,yps,zps,ms,xvs,yvs,zvs,xas,yas,zas,angle
    start = time.time()
    (xps,yps,zps,ms,xvs,yvs,zvs,xas,yas,zas) = \
      step_nbody(steps_per_call, epsilon, time_step, xps,yps,zps,ms,xvs,yvs,zvs,xas,yas,zas)
    frame = render_nbody(width, height, x_ul, y_ul, x_br, y_br, xps, yps, zps, x_rotation, y_rotation).get()
    end = time.time()
    pygame.surfarray.blit_array(surface, frame)
    screen.blit(surface, (0, 0))

    speedmessage = "Futhark call took %.2fms (N=%d) %s" % \
                   ((end-start)*1000, N, "(paused)" if time_step == 0 else "")
    showText(speedmessage, (10, 10))

    pygame.display.flip()

def point(x,y,z):
    return nb.rotatePoint(x, y, z, -x_rotation, -y_rotation)

def BOOM(pos):
    global N, xps,yps,zps,ms,xvs,yvs,zvs,xas,yas,zas
    x_dist = float(x_br-x_ul)
    y_dist = float(y_br-y_ul)
    x,y = pos
    x = x_ul + (float(x) / width) * x_dist
    y = y_ul + (float(y) / height) * y_dist
    (x,y,z) = point(x,y,0)
    BOOM_N = 100

    angles = numpy.random.rand(BOOM_N).astype('float32') * numpy.pi * 2
    lengths = numpy.random.rand(BOOM_N).astype('float32')

    xps = numpy.concatenate((xps.get(), x + lengths * numpy.cos(angles)))
    yps = numpy.concatenate((yps.get(), y + lengths * numpy.sin(angles)))
    zps = numpy.concatenate((zps.get(), z + lengths * numpy.sin(angles)))
    ms = numpy.concatenate((ms.get(), numpy.random.rand(BOOM_N).astype('float32')))
    xvs = numpy.concatenate((xvs.get(),  numpy.zeros(BOOM_N).astype('float32')))
    yvs = numpy.concatenate((yvs.get(), numpy.zeros(BOOM_N).astype('float32')))
    zvs = numpy.concatenate((zvs.get(), numpy.zeros(BOOM_N).astype('float32')))
    xas = numpy.concatenate((xas.get(), numpy.zeros(BOOM_N).astype('float32')))
    yas = numpy.concatenate((yas.get(), numpy.zeros(BOOM_N).astype('float32')))
    zas = numpy.concatenate((zas.get(), numpy.zeros(BOOM_N).astype('float32')))

    N += BOOM_N

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

def moveLeft():
    global x_ul, x_br
    x_dist = abs(x_br-x_ul)
    x_ul -= x_dist * 0.01
    x_br -= x_dist * 0.01

def moveRight():
    global x_ul, x_br
    x_dist = abs(x_br-x_ul)
    x_ul += x_dist * 0.01
    x_br += x_dist * 0.01

def moveUp():
    global y_ul, y_br
    y_dist = abs(y_br-x_ul)
    y_ul -= y_dist * 0.01
    y_br -= y_dist * 0.01

def moveDown():
    global y_ul, y_br
    y_dist = abs(y_br-x_ul)
    y_ul += y_dist * 0.01
    y_br += y_dist * 0.01

mass_active = False
def mouseMass(pos):
    global ms, xps, yps, zps

    x_dist = float(x_br-x_ul)
    y_dist = float(y_br-y_ul)
    x,y = pos
    x = x_ul + (float(x) / width) * x_dist
    y = y_ul + (float(y) / height) * y_dist
    (x,y,_) = point(x,y,0)

    if mass_active and pos:
        ms[0] = 10000
        xps[0] = x
        yps[0] = y
        zps[0] = 0
        xvs[0] = 0
        yvs[0] = 0
        zvs[0] = 0
    else:
        ms[0] = 0.0001
        xps[0] = x_br
        yps[0] = y_br


pygame.key.set_repeat(1, 1)
time_step = default_time_step
while True:
    curtime += 0.005
    render(time_step)
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
                        moveRight()
                    if event.key == pygame.K_LEFT:
                        moveLeft()
                    if event.key == pygame.K_UP:
                        moveUp()
                    if event.key == pygame.K_DOWN:
                        moveDown()
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
        elif event.type == pygame.KEYUP:
            if event.key == pygame.K_SPACE:
                if time_step > 0:
                    time_step = 0
                else:
                    time_step = default_time_step
            if event.key == pygame.K_HOME:
                resetPos()
        elif event.type == pygame.MOUSEBUTTONDOWN:
            if pygame.mouse.get_pressed()[0]:
                BOOM(pygame.mouse.get_pos())
            mass_active = pygame.mouse.get_pressed()[2] == 1
        elif event.type == pygame.MOUSEBUTTONUP:
            mass_active = pygame.mouse.get_pressed()[2] == 1
    mouseMass(pygame.mouse.get_pos())

#!/usr/bin/env python

import nbody

import numpy
import pygame
import time
import sys

width=1200
height=800
size=(width,height)
(x_ul, y_ul, x_br, y_br) = (0.0, 0.0, width, height)
time_step = 0.2

nb = nbody.nbody()
render_nbody = nb.render
step_nbody = nb.main
N = 5000
epsilon=0.50

xps = numpy.random.rand(N).astype('float32')*width
yps = numpy.random.rand(N).astype('float32')*height
zps = numpy.random.rand(N).astype('float32')
ms = numpy.random.rand(N).astype('float32')
xvs = numpy.zeros(N).astype('float32')
yvs = numpy.zeros(N).astype('float32')
zvs = numpy.zeros(N).astype('float32')
xas = numpy.zeros(N).astype('float32')
yas = numpy.zeros(N).astype('float32')
zas = numpy.zeros(N).astype('float32')

pygame.init()
pygame.display.set_caption('Nbody')
screen = pygame.display.set_mode(size)
surface = pygame.Surface(size)
font = pygame.font.Font(None, 36)

curtime=0

def showText(what, where):
    text = font.render(what, 1, (255, 255, 255))
    screen.blit(text, where)

def render():
    global xps,yps,zps,ms,xvs,yvs,zvs,xas,yas,zas
    start = time.time()
    (xps,yps,zps,ms,xvs,yvs,zvs,xas,yas,zas) = \
      step_nbody(1, epsilon, time_step, xps,yps,zps,ms,xvs,yvs,zvs,xas,yas,zas)
    frame = render_nbody(width, height, x_ul, y_ul, x_br, y_br, xps, yps, zps).get()
    end = time.time()
    pygame.surfarray.blit_array(surface, frame)
    screen.blit(surface, (0, 0))

    speedmessage = "Futhark call took %.2fms (N=%d)" % ((end-start)*1000, N)
    showText(speedmessage, (10, 10))

    pygame.display.flip()

def BOOM(pos):
    global N, xps,yps,zps,ms,xvs,yvs,zvs,xas,yas,zas
    x,y = pos
    BOOM_N = 100

    angles = numpy.random.rand(BOOM_N).astype('float32') * numpy.pi * 2
    lengths = numpy.random.rand(BOOM_N).astype('float32')

    xps = numpy.concatenate((xps.get(), x + lengths * numpy.cos(angles)))
    yps = numpy.concatenate((yps.get(), y + lengths * numpy.sin(angles)))
    zps = numpy.concatenate((zps.get(), numpy.zeros(BOOM_N).astype('float32')))
    ms = numpy.concatenate((ms.get(), numpy.random.rand(BOOM_N).astype('float32')))
    xvs = numpy.concatenate((xvs.get(),  numpy.zeros(BOOM_N).astype('float32')))
    yvs = numpy.concatenate((yvs.get(), numpy.zeros(BOOM_N).astype('float32')))
    zvs = numpy.concatenate((zvs.get(), numpy.zeros(BOOM_N).astype('float32')))
    xas = numpy.concatenate((xas.get(), numpy.zeros(BOOM_N).astype('float32')))
    yas = numpy.concatenate((yas.get(), numpy.zeros(BOOM_N).astype('float32')))
    zas = numpy.concatenate((zas.get(), numpy.zeros(BOOM_N).astype('float32')))

    N += BOOM_N

while True:
    curtime += 0.005
    render()
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        elif event.type == pygame.MOUSEBUTTONDOWN:
            if pygame.mouse.get_pressed()[0]:
                BOOM(pygame.mouse.get_pos())


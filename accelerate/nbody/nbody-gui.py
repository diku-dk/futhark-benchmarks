#!/usr/bin/env python

import nbody

import numpy as np
from sdl2 import *
import sdl2.ext
import time
import sys
from sdl2.sdlttf import (TTF_OpenFont, TTF_RenderText_Shaded, TTF_GetError, TTF_Init, TTF_Quit)
import ctypes

try:
    import _nbody
    from futhark_ffi.compat import FutharkCompat
    print('Using futhark-pycffi backend.')
    def futhark_object():
        return FutharkCompat(_nbody)
except ImportError:
    import nbody
    print('Using futhark-pyopencl backend.')
    def futhark_object():
        return nbody.nbody(default_threshold=1, interactive=True)

nb = futhark_object()

width = 800
height = 600

SDL_Init(SDL_INIT_EVERYTHING)
window = SDL_CreateWindow("N-body",
                          SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
		          width, height, SDL_WINDOW_SHOWN)

def reWindow(window):
    window_surface = SDL_GetWindowSurface(window)
    frame_py = np.ndarray(shape=(height, width), dtype=np.int32, order='C')
    surface = SDL_CreateRGBSurfaceFrom(frame_py.ctypes.data, width, height, 32, width*4,
                                       0xFF0000, 0xFF00, 0xFF, 0x00000000)
    return (window_surface, frame_py, surface)

(window_surface, frame_py, surface) = reWindow(window)

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
    return (np.random.normal(size=N,scale=width/5,loc=0).astype('float32'),
            np.random.normal(size=N,scale=height/5,loc=0).astype('float32'),
            np.random.normal(size=N,scale=height/5,loc=0).astype('float32'),

            np.random.rand(N).astype('float32'),

            np.zeros(N).astype('float32'),
            np.zeros(N).astype('float32'),
            np.zeros(N).astype('float32'))

def random_points_orbit():
    xs = np.random.normal(size=N,scale=width/10,loc=0).astype('float32')
    return (xs,
            np.random.normal(size=N,scale=height/200,loc=0).astype('float32') * (xs/100),
            np.random.normal(size=N,scale=height/200,loc=0).astype('float32') * (xs/10),

            np.random.rand(N).astype('float32'),

            np.zeros(N).astype('float32'),
            xs/width*10,
            np.zeros(N).astype('float32'))

def random_points_donut():
    angles = np.random.rand(N).astype('float32') * np.pi * 2
    tangents = np.pi*2 - angles
    lengthscales = np.random.normal(size=N).astype('float32')
    lengths = lengthscales*min(width,height)/10 + min(width,height)/2
    return (lengths * np.sin(angles),
            lengths * np.cos(angles),
            np.random.rand(N).astype('float32') * width / 10,

            np.random.rand(N).astype('float32'),

            np.sin(angles-np.pi/2)*2*np.abs(lengthscales),
            np.cos(angles-np.pi/2)*2*np.abs(lengthscales),
            np.zeros(N).astype('float32'))

def random_points_spiral():
    lengths = np.arange(N).astype('float32') / (N/(min(width,height)/2))
    angles = lengths / (3*np.pi)
    return (lengths * np.sin(angles),
            lengths * np.cos(angles),
            np.random.rand(N).astype('float32') * width / 10,

            np.random.rand(N).astype('float32'),

            np.zeros(N).astype('float32'),
            np.zeros(N).astype('float32'),
            np.zeros(N).astype('float32'))


(xps,yps,zps,ms,xvs,yvs,zvs) = random_points()

curtime=0

def render(time_step,invert):
    global xps,yps,zps,ms,xvs,yvs,zvs,angle
    start = time.time()
    (xps,yps,zps,ms,xvs,yvs,zvs) = \
      step_nbody(steps_per_call, epsilon, time_step, xps,yps,zps,ms,xvs,yvs,zvs)
    frame = render_nbody(height, width, x_ul, y_ul, x_br, y_br, xps, yps, zps, ms, x_rotation, y_rotation, max_mass, invert).get(ary=frame_py)
    end = time.time()
    futhark_time = (end-start)*1000
    start = time.time()
    SDL_BlitSurface(surface, None, window_surface, None)
    SDL_UpdateWindowSurface(window)
    end = time.time()
    blit_time = (end-start)*1000

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

    angles = np.random.rand(blob_N).astype('float32') * np.pi * 2
    z_angles = np.random.rand(blob_N).astype('float32') * np.pi * 2
    lengths = np.random.rand(blob_N).astype('float32') * min(width,height)/100

    xps = np.concatenate((xps.get(), x + lengths * np.cos(angles)))
    yps = np.concatenate((yps.get(), y + lengths * np.sin(angles)))
    zps = np.concatenate((zps.get(), z + lengths * np.sin(z_angles)))
    ms = np.concatenate((ms.get(), np.random.rand(blob_N).astype('float32')))
    xvs = np.concatenate((xvs.get(),  np.zeros(blob_N).astype('float32')))
    yvs = np.concatenate((yvs.get(), np.zeros(blob_N).astype('float32')))
    zvs = np.concatenate((zvs.get(), np.zeros(blob_N).astype('float32')))

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
        (xps, yps, zps, ms) = nb.mouse_mass_active(xps, yps, zps, ms, x, y, z)
    else:
        (xps, yps, zps, ms) = nb.mouse_mass_inactive(xps, yps, zps, ms)

time_step = default_time_step
time_then = time.time()

def mouse_x_y():
    x, y = ctypes.c_int(0), ctypes.c_int(0)
    buttonstate = sdl2.mouse.SDL_GetMouseState(ctypes.byref(x), ctypes.byref(y))
    return (x.value, y.value)

invert = False
while True:
    time_now = time.time()
    time_delta = time_now - time_then
    time_then = time_now

    rotating_x = 0.0
    rotating_y = 0.0

    render(time_step,invert)
    events = sdl2.ext.get_events()
    for event in events:
        if event.type == SDL_QUIT:
            sys.exit()
        elif event.type == SDL_KEYDOWN:
            key = event.key.keysym.sym
            if key == SDLK_ESCAPE:
                sys.exit()
            elif key == SDLK_PLUS:
                zoomIn()
            elif key == SDLK_MINUS:
                zoomOut()
            elif key == SDLK_SPACE:
                if time_step > 0:
                    default_time_step = time_step
                    time_step = 0
                else:
                    time_step = default_time_step
            elif key == SDLK_HOME:
                x_rotation=0.0
                y_rotation=0.0
                (x_ul, y_ul, x_br, y_br) = (-width/2, -height/2, width/2, height/2)
            elif key == SDLK_c:
                N = 1
                (xps,yps,zps,ms,xvs,yvs,zvs) = random_points()
            elif key == SDLK_r:
                (xps,yps,zps,ms,xvs,yvs,zvs) = random_points()
            elif key == SDLK_o:
                (xps,yps,zps,ms,xvs,yvs,zvs) = random_points_orbit()
            elif key == SDLK_d:
                (xps,yps,zps,ms,xvs,yvs,zvs) = random_points_donut()
            elif key == SDLK_s:
                (xps,yps,zps,ms,xvs,yvs,zvs) = random_points_spiral()
            elif key == SDLK_q:
                time_step -= 0.01
            elif key == SDLK_w:
                time_step += 0.01
            if key == SDLK_i:
                invert = not invert
            elif (event.key.keysym.mod & KMOD_SHIFT) != 0:
                if key == SDLK_RIGHT:
                    y_rotation += 0.01
                if key == SDLK_LEFT:
                    y_rotation -= 0.01
                if key == SDLK_UP:
                    x_rotation += 0.01
                if key == SDLK_DOWN:
                    x_rotation -= 0.01
            else:
                if key == SDLK_RIGHT:
                    moveRight(time_delta)
                if key == SDLK_LEFT:
                    moveLeft(time_delta)
                if key == SDLK_UP:
                    moveUp(time_delta)
                if key == SDLK_DOWN:
                    moveDown(time_delta)

        elif event.type == SDL_MOUSEBUTTONDOWN:
            button = event.button
            if button.button == 1: # left click
                blob((button.x, button.y))
            elif button.button == 3: # right click
                mass_active = True
        elif event.type == SDL_MOUSEBUTTONUP:
            mass_active = False

    mouseMass(mouse_x_y())

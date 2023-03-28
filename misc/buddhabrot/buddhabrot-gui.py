#!/usr/bin/env python

from buddhabrot import buddhabrot

import numpy as np
from sdl2 import *
import sdl2.ext
import time
import sys

buddhabrot = buddhabrot(interactive=True)
npoints = 100000

(width, height) = (1024,768)
aspect = width/float(height)
SDL_Init(SDL_INIT_EVERYTHING)
window = SDL_CreateWindow(b"Buddhabrot",
                          SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
		          width, height, SDL_WINDOW_SHOWN)


def reWindow(window):
    window_surface = SDL_GetWindowSurface(window)
    frame_py = np.ndarray(shape=(height, width), dtype=np.uint32, order='C')
    surface = SDL_CreateRGBSurfaceFrom(frame_py.ctypes.data, width, height, 32, width*4,
                                       0xFF0000, 0xFF00, 0xFF, 0x00000000)
    return (window_surface, frame_py, surface)

(window_surface, frame_py, surface) = reWindow(window)

state = buddhabrot.new_state(width, height, 123)

def render(state):
    xcentre = 0
    ycentre = -0.5
    w = 3.067
    limit = 255
    radius = 4.0
    (state, frame_fut) = buddhabrot.main(width, height, state,
                                         xcentre, ycentre, w,
                                         limit, radius, npoints)
    frame_fut.get(ary=frame_py)
    SDL_BlitSurface(surface, None, window_surface, None)
    SDL_UpdateWindowSurface(window)
    return state

running=True
last=time.time()
while running:
    state = render(state)
    last=time.time()
    events = sdl2.ext.get_events()
    for event in events:
        if event.type == SDL_QUIT:
            running=False
        if event.type == SDL_KEYDOWN:
            key = event.key.keysym.sym
            if key == SDLK_q:
                running=False
            elif key == SDLK_f:
                SDL_SetWindowFullscreen(window, SDL_WINDOW_FULLSCREEN)
                (window_surface, frame_py, surface) = reWindow(window)
            elif key == SDLK_g:
                SDL_SetWindowFullscreen(window, 0)

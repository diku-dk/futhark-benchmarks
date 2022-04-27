#!/usr/bin/env python

import life
import numpy as np
from sdl2 import *
import sdl2.ext
import argparse
import pyopencl as cl
import sys

life = life.life(interactive=True)

rulesets = [ 'conway', 'conway_fading', 'quad', 'quad_fading', 'quad2', 'rule101' ]

def get_ruleset(l, s):
    return (getattr(l, s + '_init'),
            getattr(l, s + '_steps'),
            getattr(l, s + '_render'),
            getattr(l, s + '_uninit'))

def next_ruleset(l, s, d):
    ruleset = rulesets[(rulesets.index(s) + d) % len(rulesets)]
    return ruleset, get_ruleset(l, ruleset)

parser = argparse.ArgumentParser(description='The Game of Life!')
parser.add_argument('--variant', metavar='RULES', type=str, choices=rulesets, default='conway',
                    help='The variant of the game to run')
parser.add_argument('--width', metavar='INT', type=int, default=800,
                    help='Width of the world')
parser.add_argument('--height', metavar='INT', type=int, default=600,
                    help='Height of the world')
parser.add_argument('--steps', metavar='INT', type=int, default=1,
                    help='Number of simulation steps to perform per frame')

args = parser.parse_args()
height = args.height
width = args.width

steps=args.steps

SDL_Init(SDL_INIT_EVERYTHING)
window = SDL_CreateWindow(b"Game of Life",
                          SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
		          width, height, SDL_WINDOW_SHOWN)


def reWindow(window):
    window_surface = SDL_GetWindowSurface(window)
    frame_py = np.ndarray(shape=(height, width), dtype=np.uint32, order='C')
    surface = SDL_CreateRGBSurfaceFrom(frame_py.ctypes.data, width, height, 32, width*4,
                                       0xFF0000, 0xFF00, 0xFF, 0x00000000)
    return (window_surface, frame_py, surface)

(window_surface, frame_py, surface) = reWindow(window)

initworld = np.random.choice([True, False], size=(height, width))

ruleset = args.variant
life_init, life_steps, life_render, life_uninit = get_ruleset(life, ruleset)

life_state = life_init(initworld)

def render():
    frame = life_render(life_state)
    frame.get(ary=frame_py)
    SDL_BlitSurface(surface, None, window_surface, None)
    SDL_UpdateWindowSurface(window)

def switch_rules(d):
    global ruleset, life_state, life_init, life_steps, life_render, life_uninit
    bools = life_uninit(life_state)
    ruleset, (life_init, life_steps, life_render, life_uninit) = next_ruleset(life, ruleset, d)
    life_state = life_init(bools)

running=True
while running:
    life_state = life_steps(steps, life_state)
    render()
    events = sdl2.ext.get_events()
    for event in events:
        if event.type == SDL_QUIT:
            running=False
        if event.type == SDL_KEYDOWN:
            key = event.key.keysym.sym
            if key == SDLK_RIGHT:
                switch_rules(1)
            if key == SDLK_LEFT:
                switch_rules(-1)
            if key == SDLK_ESCAPE:
                running=False
            if key == SDLK_r:
                life_state = life_init(np.random.choice([True, False],
                                                        size=(height, width)))

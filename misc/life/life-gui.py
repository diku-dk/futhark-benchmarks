#!/usr/bin/env python

import life
import numpy
import pygame
import argparse
import pyopencl as cl
import sys

rulesets = [ 'conway', 'conway_fading', 'quad', 'quad_fading', 'quad2' ]

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

steps=args.steps
size=(args.width,args.height)


screen = pygame.display.set_mode(size)
initworld = numpy.random.choice([True, False], size=size)

life = life.life()

ruleset = args.variant
life_init, life_steps, life_render, life_uninit = get_ruleset(life, ruleset)

life_state = life_init(initworld)

def render():
    frame = life_render(life_state)
    # We get back a PyOpenCL array.  It is mostly compatible with
    # Numpy, but Pygame really wants a proper Numpy array, so we use
    # the get() method to obtain that.
    pygame.surfarray.blit_array(screen, frame.get())
    pygame.display.flip()

def switch_rules(d):
    global ruleset, life_state, life_init, life_steps, life_render, life_uninit
    bools = life_uninit(life_state)
    ruleset, (life_init, life_steps, life_render, life_uninit) = next_ruleset(life, ruleset, d)
    life_state = life_init(bools)

while True:
    life_state = life_steps(steps, life_state)
    render()
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_RIGHT:
                switch_rules(1)
            if event.key == pygame.K_LEFT:
                switch_rules(-1)

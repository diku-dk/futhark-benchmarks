#!/usr/bin/env python

import life
import quadlife
import numpy
import pygame
import argparse
import pyopencl as cl
import sys

rulesets = {
    'life' : life.life,
    'quadlife' : quadlife.quadlife
}

parser = argparse.ArgumentParser(description='The Game of Life!')
parser.add_argument('--variant', metavar='RULES', type=str, choices=rulesets.keys(), default='life',
                    help='The variant of the game to run')
parser.add_argument('--width', metavar='INT', type=int, default=800,
                    help='Width of the world')
parser.add_argument('--height', metavar='INT', type=int, default=600,
                    help='Height of the world')
parser.add_argument('--steps', metavar='INT', type=int, default=3,
                    help='Number of simulation steps to perform per frame')

args = parser.parse_args()

steps=args.steps
size=(args.width,args.height)


l = rulesets[args.variant]()
screen = pygame.display.set_mode(size)
initworld = numpy.random.choice([True, False], size=size)
world, history = l.init(initworld)
surface = pygame.Surface(size)

def render():
    frame = l.render_frame(history)
    pygame.surfarray.blit_array(surface, frame)
    screen.blit(surface, (0, 0))
    pygame.display.flip()

while True:
    world, history = l.steps(world, history, steps)
    render()
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()

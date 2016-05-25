#!/usr/bin/env python

import sys
import time
import random

import pyopencl
import numpy as np
import pygame

import hotspot

# From http://www.roguebasin.com/index.php?title=Bresenham%27s_Line_Algorithm#Python
def get_line(start, end):
    """Bresenham's Line Algorithm
    Produces a list of tuples from start and end

    >>> points1 = get_line((0, 0), (3, 4))
    >>> points2 = get_line((3, 4), (0, 0))
    >>> assert(set(points1) == set(points2))
    >>> print points1
    [(0, 0), (1, 1), (1, 2), (2, 3), (3, 4)]
    >>> print points2
    [(3, 4), (2, 3), (1, 2), (1, 1), (0, 0)]
    """
    # Setup initial conditions
    x1, y1 = start
    x2, y2 = end
    dx = x2 - x1
    dy = y2 - y1

    # Determine how steep the line is
    is_steep = abs(dy) > abs(dx)

    # Rotate line
    if is_steep:
        x1, y1 = y1, x1
        x2, y2 = y2, x2

    # Swap start and end points if necessary and store swap state
    swapped = False
    if x1 > x2:
        x1, x2 = x2, x1
        y1, y2 = y2, y1
        swapped = True

    # Recalculate differentials
    dx = x2 - x1
    dy = y2 - y1

    # Calculate error
    error = int(dx / 2.0)
    ystep = 1 if y1 < y2 else -1

    # Iterate over bounding box generating points between start and end
    y = y1
    points = []
    for x in range(x1, x2 + 1):
        coord = (y, x) if is_steep else (x, y)
        points.append(coord)
        error -= abs(dy)
        if error < 0:
            y += ystep
            error += dx

    # Reverse the list if the coordinates were swapped
    if swapped:
        points.reverse()
    return points

class HotSpotQuit(Exception):
    pass

# Two program states
DEFINING_POWER = 0
SIMULATING_TEMPERATURE = 1

def slurp(file):
    with open(file, 'r') as myfile:
        data=myfile.read()
        return data

class HotSpot:
    def __init__(self, grid_resolution):
        self.state = DEFINING_POWER
        self.simulation = hotspot.hotspot()

        self.grid_resolution = grid_resolution
        self.iterations_per_step = 1

        self.making_temp = False
        self.pos_old = None

    def run(self):
        g = self.grid_resolution
        self.power = np.zeros((g, g), dtype=np.float32)

        # Arbitrary random initial temperatures.
        self.temp = 200 + np.random.rand(g,g).astype(np.float32) * 160

        pygame.init()
        pygame.display.set_caption('HotSpot!')
        size = (self.grid_resolution, self.grid_resolution)
        self.screen = pygame.display.set_mode(size)
        self.surface = pygame.Surface(size)
        self.font = pygame.font.Font(None, 26)

        try:
            self.loop()
        except HotSpotQuit:
            return

    def new_frame(self):
        self.temp = self.simulation.compute_tran_temp(self.iterations_per_step, self.temp, self.power)

        return self.simulation.render_frame(self.temp).get()

    def add_power(self, pos_old, pos_new):
        x_old, y_old = pos_old
        x_new, y_new = pos_new
        points = get_line((x_old+1, y_old+1),
                          (x_new+1, y_new+1))

        for x,y in points:
            self.power[x,y] = 1.0

    def react_power(self):
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                raise HotSpotQuit()
            elif event.type == pygame.MOUSEBUTTONDOWN:
                self.making_temp = True
            elif event.type == pygame.MOUSEBUTTONUP:
                self.making_temp = False
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_SPACE:
                    self.state = SIMULATING_TEMPERATURE
                    return

        pos_new = pygame.mouse.get_pos()

        if self.pos_old is not None:
            if self.making_temp:
                self.add_power(self.pos_old, pos_new)

        self.pos_old = pos_new

    def react_temp(self):
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                raise HotSpotQuit()
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_SPACE:
                    self.state = DEFINING_POWER
                    return

    def react(self):
        if self.state == DEFINING_POWER:
            self.react_power()
        elif self.state == SIMULATING_TEMPERATURE:
            self.react_temp()

    def render_power(self):
        g = self.grid_resolution
        frame = np.zeros((g, g, 3), dtype=np.int8)
        frame[self.power > 0] = [255, 255, 255]
        pygame.surfarray.blit_array(self.surface, frame)
        self.screen.blit(self.surface, (0, 0))
        self.show_text('Draw with the mouse.  Press Space to run', (10,10))
        pygame.display.flip()

    def render_temp(self):
        start = time.time()
        frame = self.new_frame()
        end = time.time()
        diff_ms = (end - start) * 1000.0

        pygame.surfarray.blit_array(self.surface, frame)
        self.screen.blit(self.surface, (0, 0))
        self.show_text('Futhark call took {:.2f} ms.'.format(diff_ms), (10, 10))
        pygame.display.flip()

    def render(self):
        if self.state == DEFINING_POWER:
            self.render_power()
        elif self.state == SIMULATING_TEMPERATURE:
            self.render_temp()

    def show_text(self, what, where, color=(0, 255, 0), antialias=True):
        text = self.font.render(what, antialias, color)
        self.screen.blit(text, where)

    def loop(self):
        while True:
            self.react()
            self.render()


def main(args):
    try:
        grid_resolution = int(args[0])
    except IndexError:
        print 'Usage: ./hotspot-gui.py GRID_RESOLUTION'
        return 1
    f = HotSpot(grid_resolution=grid_resolution)
    f.run()
    return 0

if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))

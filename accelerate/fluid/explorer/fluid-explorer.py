#!/usr/bin/env python

import sys
import time
import random

import numpy as np
import pygame

import fluid


class FluidQuit(Exception):
    pass


class FluidExplorer:
    def __init__(self, grid_resolution):
        self.grid_resolution = grid_resolution
        self.time_step = 0.1
        self.n_solver_steps = 20
        self.diffusion_rate = 0  # 0.0001
        self.viscosity = 0  # 0.00001

        self.pos_old = None
        self.making_densities = False
        self.making_forces = False

    def run(self):
        g = self.grid_resolution + 2
        self.U = np.zeros((g, g), dtype=np.float32)
        self.V = np.zeros((g, g), dtype=np.float32)
        self.D = np.zeros((g, g), dtype=np.float32)

        self.futhark = fluid.fluid_visualize_densities_one_frame_rgb()

        pygame.init()
        pygame.display.set_caption('Fluid Simulation Explorer!')
        size = (self.grid_resolution, self.grid_resolution)
        self.screen = pygame.display.set_mode(size)
        self.surface = pygame.Surface(size)
        self.font = pygame.font.Font(None, 26)

        try:
            self.loop()
        except FluidQuit:
            return

    def new_frame(self):
        frame, U1, V1, D1 = self.futhark.main(
            self.U, self.V, self.D,
            self.n_solver_steps, self.time_step,
            self.diffusion_rate, self.viscosity)
        self.U = U1
        self.V = V1
        self.D = D1
        return frame

    def add_force(self, pos_old, pos_new):
        x0, y0 = pos_old
        x1, y1 = pos_new
        force = 5.0
        self.U[x1, y1] = force * (x1 - x0)
        self.V[x1, y1] = force * (y1 - y0)

    def add_density(self, pos):
        x, y = pos
        source = 100.0
        x_ = x + 1
        y_ = y + 1
        self.D[x_, y_] = source

    def react(self):
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                raise FluidQuit()
            elif event.type == pygame.MOUSEBUTTONDOWN:
                if pygame.mouse.get_pressed()[0]:
                    self.making_forces = True
                    self.making_densities = False
                elif pygame.mouse.get_pressed()[2]:
                    self.making_densities = True
                    self.making_forces = False
            elif event.type == pygame.MOUSEBUTTONUP:
                self.making_forces = False
                self.making_densities = False

        pos_new = pygame.mouse.get_pos()

        if self.making_densities:
            self.add_density(pos_new)
        elif self.making_forces and self.pos_old is not None:
            self.add_force(self.pos_old, pos_new)
            
        self.pos_old = pos_new

    def render(self):
        start = time.time()
        frame = self.new_frame()
        end = time.time()
        diff_ms = (end - start) * 1000.0
        pygame.surfarray.blit_array(self.surface, frame)
        self.screen.blit(self.surface, (0, 0))
        self.show_text('Futhark call took {:.2f} ms.'.format(diff_ms), (0, 0))
        pygame.display.flip()

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
        print 'Usage: ./fluid-explorer.py GRID_RESOLUTION'
        print
        print 'Add densities with right click.  Add forces with left click.'
        print
        print 'Example: Create a 256x256 fluid simulation.'
        print '  ./fluid-explorer.py 256'
        return 1
    f = FluidExplorer(grid_resolution=grid_resolution)
    f.run()
    return 0
            
if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))

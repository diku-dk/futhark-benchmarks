#!/usr/bin/env python3

import tkinter as tk
from tkinter import filedialog
from scipy import misc
from srad import srad
import numpy as np
import pygame
import time
import sys

srad = srad()

root = tk.Tk()
root.withdraw()
file_path = filedialog.askopenfilename()

if type(file_path) != str:
    exit(0)

try:
    img = misc.imread(file_path, mode='L')
except:
    print('Cannot load {} as image.'.format(file_path))
    exit(1)

size = (img.shape[1], img.shape[0])
pygame.init()
pygame.display.set_caption('SRAD')
screen = pygame.display.set_mode(size)
surface = pygame.Surface(size)
font = pygame.font.Font(None, 36)
pygame.key.set_repeat(100, 100)

def showText(what, where):
    text = font.render(what, 1, (255, 255, 255))
    screen.blit(text, where)

niters=0
l=0.5

while True:
    start = time.time()
    frame = np.transpose(srad.srad(niters, l, img).get())
    end = time.time()
    pygame.surfarray.blit_array(surface, frame)
    screen.blit(surface, (0, 0))
    showText('Futhark call took %.2fms' % ((end-start)*1000), (10, 10))
    showText('iters: %d; lambda: %.2f    change with arrow keys' % (niters, l), (10, 30))
    pygame.display.flip()

    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_RIGHT:
                niters += 1
            if event.key == pygame.K_LEFT:
                niters -= 1
            if event.key == pygame.K_UP:
                l += 0.01
            if event.key == pygame.K_DOWN:
                l -= 0.01

LYS_BACKEND?=opencl
LYS_FRONTEND?=sdl
LYS_TTF?=0

SELF_DIR := $(dir $(lastword $(MAKEFILE_LIST)))

ifeq ($(origin PROG_FUT_DEPS), undefined)
PROG_FUT_DEPS:=$(shell ls *.fut; find $(SELF_DIR)/../../.. -name \*.fut)
endif

ifeq ($(LYS_FRONTEND),sdl)
PKG_CFLAGS_PKGS=sdl2
ifeq ($(LYS_TTF),1)
PKG_CFLAGS_PKGS+= SDL2_ttf
endif

else ifeq ($(LYS_FRONTEND),ncurses)
PKG_CFLAGS_PKGS=ncurses

else
$(error Unknown LYS_FRONTEND: $(LYS_FRONTEND).  Must be 'sdl' or 'ncurses')
endif

PKG_CFLAGS=$(shell pkg-config --cflags $(PKG_CFLAGS_PKGS))
PKG_LDFLAGS=$(shell pkg-config --libs $(PKG_CFLAGS_PKGS))

NOWARN_CFLAGS=-std=c11 -O

CFLAGS?=$(NOWARN_CFLAGS) $(PKG_CFLAGS) -Wall -Wextra -pedantic

ifeq ($(LYS_TTF),1)
CFLAGS+= -DLYS_TTF
endif

ifeq ($(LYS_BACKEND),opencl)
OS=$(shell uname -s)
ifeq ($(OS),Darwin)
DEVICE_LDFLAGS=-framework OpenCL
else
DEVICE_LDFLAGS=-lOpenCL
endif
else ifeq ($(LYS_BACKEND),cuda)
DEVICE_LDFLAGS=-lcuda -lnvrtc -lcudart
else ifeq ($(LYS_BACKEND),hip)
DEVICE_LDFLAGS=-lamdhip64 -lhiprtc
else ifeq ($(LYS_BACKEND),c)
DEVICE_LDFLAGS=
else ifeq ($(LYS_BACKEND),multicore)
DEVICE_LDFLAGS=-lpthread
else
$(error Unknown LYS_BACKEND: $(LYS_BACKEND).  Must be 'opencl', 'cuda', 'hip', 'multicore', or 'c')
endif

LDFLAGS?=-lm $(PKG_LDFLAGS) $(DEVICE_LDFLAGS)

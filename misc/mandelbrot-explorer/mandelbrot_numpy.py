from numpy import *
import pylab

# Note: very slow.

def mandelbrot(w, h, limit, minx, miny, maxx, maxy):
        '''Returns an image of the Mandelbrot fractal of size (h,w).
        '''
        y,x = ogrid[ miny:maxy:h*1j, minx:maxx:w*1j ]
        c = x+y*1j
        z = c
        divtime = limit + zeros(z.shape, dtype=int)

        for i in xrange(limit):
                z  = z**2 + c
                diverge = z*conj(z) > 2**2            # who is diverging
                div_now = diverge & (divtime==limit)  # who is diverging now
                divtime[div_now] = i                  # note when
                z[diverge] = 2                        # avoid diverging too much

        # We do not even colour it here, but merely treat the
        # divergence count as a number.
        return transpose(divtime)

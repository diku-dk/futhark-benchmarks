Canny Edge Detection
==

This is a port of the [Accelerate example
'canny'](https://github.com/AccelerateHS/accelerate-examples/blob/master/examples/canny/).
We do not implement the final recursive "Wildfire" stage (but that is
also not part of the benchmark part of the Accelerate example).

It's mostly a bunch of 2D stencils, followed by a scan+scatter at the
end.  Since we do not optimise stencils, there is little to be done
here.  Not even many opportunities for fusion.

## Runtime results on gpu01-diku-apl (GTX 780 Ti)

    lena256.bmp:
      Accelerate: 1.498ms
      Futhark:    0.791ms
      Speedup:    1.89x

    lena512.bmp:
      Accelerate: 2.683ms
      Futhark:    2.099ms
      Speedup:    1.27x

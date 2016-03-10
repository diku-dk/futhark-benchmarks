#!/usr/bin/env python3
#
# Transform pre-futharkified Accelerate dataset to parseable Futhark input (yes,
# it's messy).

import sys
import re

with open(sys.argv[1]) as f:
    d = f.read()

d = d.replace('{', '(').replace('}', ')')
d = re.sub(r'(([0-9]|-)[0-9]*\.[0-9]*([eE][-0-9]+)?)', r'"\1"', d)
d = eval(d)
d = list(zip(*d))
d = '\n'.join('[' + ', '.join(f + 'f32' for f in e) + ']' for e in d)

print(d)

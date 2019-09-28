#!/usr/bin/env python3

import sys
import re

out_file, in_file = sys.argv[1:]

with open(in_file) as f:
    contents = f.read()

start = contents.find('futhark_entry_text_content')
end = contents.find(')', start)
types = re.findall('([^ ]+) \*out\d+,', contents[start:end])
out_vars = ['out{}'.format(i) for i in range(len(types))]

with open(out_file, 'w') as f:
    print('#include <stdio.h>', file=f)
    print('#include "lib/github.com/diku-dk/lys/liblys.h"', file=f)
    if len(types) == 0:
        print('#define UNUSED(x) (void)(x)', file=f)
    print('void build_text(const struct lys_context *ctx, char* dest, size_t dest_len, const char* format, float render_milliseconds) {', file=f)
    if len(types) > 0:
        for t, v in zip(types, out_vars):
            print('  {} {};'.format(t, v), file=f)
        print('  FUT_CHECK(ctx->fut, futhark_entry_text_content(ctx->fut, {}, render_milliseconds, ctx->state));'.format(', '.join('&' + v for v in out_vars)), file=f)
        print('  snprintf(dest, dest_len, format, {});'.format(', '.join(out_vars)), file=f)
    else:
        for x in ['ctx', 'render_milliseconds']:
            print('UNUSED({});'.format(x), file=f)
        print('  snprintf(dest, dest_len, "%s", format);', file=f)
    print('}', file=f)

#!/usr/bin/env python3

import sys
import re
import json

self_dir, out_file, in_file = sys.argv[1:]

with open(in_file) as f:
    manifest = json.load(f)

contents_type = manifest['entry_points']['text_content']['output']['type']

C_TYPE_MAPPING = { 'u8': 'uint8_t',
                   'u16': 'uint16_t',
                   'u32': 'uint32_t',
                   'u64': 'uint64_t',
                   'i8': 'int8_t',
                   'i16': 'int16_t',
                   'i32': 'int32_t',
                   'i64': 'int64_t',
                   'f16': 'uint16_t',
                   'f32': 'float',
                   'f64': 'double',
                   'bool': 'bool'
                  }

def to_c_type(t):
    if t in manifest['types']:
        return manifest['types'][t]['ctype']
    else:
        return C_TYPE_MAPPING[t]

contents_ctype = to_c_type(contents_type)

if contents_type in manifest['types']:
    fields=manifest['types'][contents_type]['record']['fields']
    n_printf_arguments = len(fields)
    types = [ to_c_type(f['type']) for f in fields ]
    projects = [ f['project'] for f in fields ]
    free = manifest['types'][contents_type]['ops']['free']
else:
    n_printf_arguments = 1
    types = [contents_type]
    projects=[]
    free=None

out_vars = ['out{}'.format(i) for i in range(len(types))]

with open(out_file, 'w') as f:
    print('#include <stdio.h>', file=f)
    print('#include "{}/liblys.h"'.format(self_dir), file=f)
    print('', file=f)
    if len(types) == 0:
        print('#define UNUSED(x) (void)(x)', file=f)
    print('static void build_text(const struct lys_context *ctx, char* dest, size_t dest_len, const char* format, float render_milliseconds, char* **sum_names) {', file=f)
    if len(types) > 0:
        print('  {} contents;'.format(contents_ctype), file=f)
        print('  FUT_CHECK(ctx->fut, futhark_entry_text_content(ctx->fut, &contents, render_milliseconds, ctx->state));', file=f)
        for p, t, v in zip(projects, types, out_vars):
            print(f'  union {{ {t} val; char* sum_name; }} {v};', file=f)
            print(f'  FUT_CHECK(ctx->fut, {p}(ctx->fut, &{v}.val, contents));', file=f)
        print(f'  FUT_CHECK(ctx->fut, {free}(ctx->fut, contents));', file=f)
        for v, i in zip(out_vars, range(len(out_vars))):
            print('  if (sum_names[{}] != NULL) {{'.format(i), file=f)
            print('    {v}.sum_name = sum_names[{i}][(int32_t) {v}.val];'.format(v=v, i=i), file=f)
            print('  }', file=f)
        print('  snprintf(dest, dest_len, format, {});'.format(', '.join((s + ('.sum_name' if t == 'i32' else '.val')) for s, t in zip(out_vars, types))), file=f)
    else:
        for x in ['ctx', 'render_milliseconds', 'sum_names']:
            print('UNUSED({});'.format(x), file=f)
        print('  snprintf(dest, dest_len, "%s", format);', file=f)
    print('}', file=f)
    print('', file=f)
    print('size_t n_printf_arguments() {', file=f)
    print('  return {};'.format(len(types)), file=f)
    print('}', file=f)

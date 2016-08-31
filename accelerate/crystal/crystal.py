import sys
import numpy as np
import ctypes as ct
# Stub code for OpenCL setup.

import pyopencl as cl

def get_prefered_context(interactive=False, platform_pref=None, device_pref=None):
    if interactive:
        return cl.create_some_context(interactive=True)

    def platform_ok(p):
        return not platform_pref or p.name.find(platform_pref) >= 0
    def device_ok(d):
        return not device_pref or d.name.find(device_pref) >= 0

    for p in cl.get_platforms():
        if not platform_ok(p):
            continue
        for d in p.get_devices():
            if not device_ok(d):
                continue
            return cl.Context(devices=[d])
    raise Exception('No OpenCL platform and device matching constraints found.')
import pyopencl.array
import time
import argparse
FUT_BLOCK_DIM = "16"
cl_group_size = np.int32(256)
cl_num_groups = np.int32(128)
synchronous = False
preferred_platform = None
preferred_device = None
fut_opencl_src = """typedef char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef long int64_t;
typedef uchar uint8_t;
typedef ushort uint16_t;
typedef uint uint32_t;
typedef ulong uint64_t;
static inline int8_t add8(int8_t x, int8_t y)
{
    return x + y;
}
static inline int16_t add16(int16_t x, int16_t y)
{
    return x + y;
}
static inline int32_t add32(int32_t x, int32_t y)
{
    return x + y;
}
static inline int64_t add64(int64_t x, int64_t y)
{
    return x + y;
}
static inline int8_t sub8(int8_t x, int8_t y)
{
    return x - y;
}
static inline int16_t sub16(int16_t x, int16_t y)
{
    return x - y;
}
static inline int32_t sub32(int32_t x, int32_t y)
{
    return x - y;
}
static inline int64_t sub64(int64_t x, int64_t y)
{
    return x - y;
}
static inline int8_t mul8(int8_t x, int8_t y)
{
    return x * y;
}
static inline int16_t mul16(int16_t x, int16_t y)
{
    return x * y;
}
static inline int32_t mul32(int32_t x, int32_t y)
{
    return x * y;
}
static inline int64_t mul64(int64_t x, int64_t y)
{
    return x * y;
}
static inline uint8_t udiv8(uint8_t x, uint8_t y)
{
    return x / y;
}
static inline uint16_t udiv16(uint16_t x, uint16_t y)
{
    return x / y;
}
static inline uint32_t udiv32(uint32_t x, uint32_t y)
{
    return x / y;
}
static inline uint64_t udiv64(uint64_t x, uint64_t y)
{
    return x / y;
}
static inline uint8_t umod8(uint8_t x, uint8_t y)
{
    return x % y;
}
static inline uint16_t umod16(uint16_t x, uint16_t y)
{
    return x % y;
}
static inline uint32_t umod32(uint32_t x, uint32_t y)
{
    return x % y;
}
static inline uint64_t umod64(uint64_t x, uint64_t y)
{
    return x % y;
}
static inline int8_t sdiv8(int8_t x, int8_t y)
{
    int8_t q = x / y;
    int8_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int16_t sdiv16(int16_t x, int16_t y)
{
    int16_t q = x / y;
    int16_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int32_t sdiv32(int32_t x, int32_t y)
{
    int32_t q = x / y;
    int32_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int64_t sdiv64(int64_t x, int64_t y)
{
    int64_t q = x / y;
    int64_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int8_t smod8(int8_t x, int8_t y)
{
    int8_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int16_t smod16(int16_t x, int16_t y)
{
    int16_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int32_t smod32(int32_t x, int32_t y)
{
    int32_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int64_t smod64(int64_t x, int64_t y)
{
    int64_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int8_t squot8(int8_t x, int8_t y)
{
    return x / y;
}
static inline int16_t squot16(int16_t x, int16_t y)
{
    return x / y;
}
static inline int32_t squot32(int32_t x, int32_t y)
{
    return x / y;
}
static inline int64_t squot64(int64_t x, int64_t y)
{
    return x / y;
}
static inline int8_t srem8(int8_t x, int8_t y)
{
    return x % y;
}
static inline int16_t srem16(int16_t x, int16_t y)
{
    return x % y;
}
static inline int32_t srem32(int32_t x, int32_t y)
{
    return x % y;
}
static inline int64_t srem64(int64_t x, int64_t y)
{
    return x % y;
}
static inline uint8_t shl8(uint8_t x, uint8_t y)
{
    return x << y;
}
static inline uint16_t shl16(uint16_t x, uint16_t y)
{
    return x << y;
}
static inline uint32_t shl32(uint32_t x, uint32_t y)
{
    return x << y;
}
static inline uint64_t shl64(uint64_t x, uint64_t y)
{
    return x << y;
}
static inline uint8_t lshr8(uint8_t x, uint8_t y)
{
    return x >> y;
}
static inline uint16_t lshr16(uint16_t x, uint16_t y)
{
    return x >> y;
}
static inline uint32_t lshr32(uint32_t x, uint32_t y)
{
    return x >> y;
}
static inline uint64_t lshr64(uint64_t x, uint64_t y)
{
    return x >> y;
}
static inline int8_t ashr8(int8_t x, int8_t y)
{
    return x >> y;
}
static inline int16_t ashr16(int16_t x, int16_t y)
{
    return x >> y;
}
static inline int32_t ashr32(int32_t x, int32_t y)
{
    return x >> y;
}
static inline int64_t ashr64(int64_t x, int64_t y)
{
    return x >> y;
}
static inline uint8_t and8(uint8_t x, uint8_t y)
{
    return x & y;
}
static inline uint16_t and16(uint16_t x, uint16_t y)
{
    return x & y;
}
static inline uint32_t and32(uint32_t x, uint32_t y)
{
    return x & y;
}
static inline uint64_t and64(uint64_t x, uint64_t y)
{
    return x & y;
}
static inline uint8_t or8(uint8_t x, uint8_t y)
{
    return x | y;
}
static inline uint16_t or16(uint16_t x, uint16_t y)
{
    return x | y;
}
static inline uint32_t or32(uint32_t x, uint32_t y)
{
    return x | y;
}
static inline uint64_t or64(uint64_t x, uint64_t y)
{
    return x | y;
}
static inline uint8_t xor8(uint8_t x, uint8_t y)
{
    return x ^ y;
}
static inline uint16_t xor16(uint16_t x, uint16_t y)
{
    return x ^ y;
}
static inline uint32_t xor32(uint32_t x, uint32_t y)
{
    return x ^ y;
}
static inline uint64_t xor64(uint64_t x, uint64_t y)
{
    return x ^ y;
}
static inline char ult8(uint8_t x, uint8_t y)
{
    return x < y;
}
static inline char ult16(uint16_t x, uint16_t y)
{
    return x < y;
}
static inline char ult32(uint32_t x, uint32_t y)
{
    return x < y;
}
static inline char ult64(uint64_t x, uint64_t y)
{
    return x < y;
}
static inline char ule8(uint8_t x, uint8_t y)
{
    return x <= y;
}
static inline char ule16(uint16_t x, uint16_t y)
{
    return x <= y;
}
static inline char ule32(uint32_t x, uint32_t y)
{
    return x <= y;
}
static inline char ule64(uint64_t x, uint64_t y)
{
    return x <= y;
}
static inline char slt8(int8_t x, int8_t y)
{
    return x < y;
}
static inline char slt16(int16_t x, int16_t y)
{
    return x < y;
}
static inline char slt32(int32_t x, int32_t y)
{
    return x < y;
}
static inline char slt64(int64_t x, int64_t y)
{
    return x < y;
}
static inline char sle8(int8_t x, int8_t y)
{
    return x <= y;
}
static inline char sle16(int16_t x, int16_t y)
{
    return x <= y;
}
static inline char sle32(int32_t x, int32_t y)
{
    return x <= y;
}
static inline char sle64(int64_t x, int64_t y)
{
    return x <= y;
}
static inline int8_t pow8(int8_t x, int8_t y)
{
    int8_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int16_t pow16(int16_t x, int16_t y)
{
    int16_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int32_t pow32(int32_t x, int32_t y)
{
    int32_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int64_t pow64(int64_t x, int64_t y)
{
    int64_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int8_t sext_i8_i8(int8_t x)
{
    return x;
}
static inline int16_t sext_i8_i16(int8_t x)
{
    return x;
}
static inline int32_t sext_i8_i32(int8_t x)
{
    return x;
}
static inline int64_t sext_i8_i64(int8_t x)
{
    return x;
}
static inline int8_t sext_i16_i8(int16_t x)
{
    return x;
}
static inline int16_t sext_i16_i16(int16_t x)
{
    return x;
}
static inline int32_t sext_i16_i32(int16_t x)
{
    return x;
}
static inline int64_t sext_i16_i64(int16_t x)
{
    return x;
}
static inline int8_t sext_i32_i8(int32_t x)
{
    return x;
}
static inline int16_t sext_i32_i16(int32_t x)
{
    return x;
}
static inline int32_t sext_i32_i32(int32_t x)
{
    return x;
}
static inline int64_t sext_i32_i64(int32_t x)
{
    return x;
}
static inline int8_t sext_i64_i8(int64_t x)
{
    return x;
}
static inline int16_t sext_i64_i16(int64_t x)
{
    return x;
}
static inline int32_t sext_i64_i32(int64_t x)
{
    return x;
}
static inline int64_t sext_i64_i64(int64_t x)
{
    return x;
}
static inline uint8_t zext_i8_i8(uint8_t x)
{
    return x;
}
static inline uint16_t zext_i8_i16(uint8_t x)
{
    return x;
}
static inline uint32_t zext_i8_i32(uint8_t x)
{
    return x;
}
static inline uint64_t zext_i8_i64(uint8_t x)
{
    return x;
}
static inline uint8_t zext_i16_i8(uint16_t x)
{
    return x;
}
static inline uint16_t zext_i16_i16(uint16_t x)
{
    return x;
}
static inline uint32_t zext_i16_i32(uint16_t x)
{
    return x;
}
static inline uint64_t zext_i16_i64(uint16_t x)
{
    return x;
}
static inline uint8_t zext_i32_i8(uint32_t x)
{
    return x;
}
static inline uint16_t zext_i32_i16(uint32_t x)
{
    return x;
}
static inline uint32_t zext_i32_i32(uint32_t x)
{
    return x;
}
static inline uint64_t zext_i32_i64(uint32_t x)
{
    return x;
}
static inline uint8_t zext_i64_i8(uint64_t x)
{
    return x;
}
static inline uint16_t zext_i64_i16(uint64_t x)
{
    return x;
}
static inline uint32_t zext_i64_i32(uint64_t x)
{
    return x;
}
static inline uint64_t zext_i64_i64(uint64_t x)
{
    return x;
}
static inline float fdiv32(float x, float y)
{
    return x / y;
}
static inline float fadd32(float x, float y)
{
    return x + y;
}
static inline float fsub32(float x, float y)
{
    return x - y;
}
static inline float fmul32(float x, float y)
{
    return x * y;
}
static inline float fpow32(float x, float y)
{
    return pow(x, y);
}
static inline char cmplt32(float x, float y)
{
    return x < y;
}
static inline char cmple32(float x, float y)
{
    return x <= y;
}
static inline float sitofp_i8_f32(int8_t x)
{
    return x;
}
static inline float sitofp_i16_f32(int16_t x)
{
    return x;
}
static inline float sitofp_i32_f32(int32_t x)
{
    return x;
}
static inline float sitofp_i64_f32(int64_t x)
{
    return x;
}
static inline float uitofp_i8_f32(uint8_t x)
{
    return x;
}
static inline float uitofp_i16_f32(uint16_t x)
{
    return x;
}
static inline float uitofp_i32_f32(uint32_t x)
{
    return x;
}
static inline float uitofp_i64_f32(uint64_t x)
{
    return x;
}
static inline int8_t fptosi_f32_i8(float x)
{
    return x;
}
static inline int16_t fptosi_f32_i16(float x)
{
    return x;
}
static inline int32_t fptosi_f32_i32(float x)
{
    return x;
}
static inline int64_t fptosi_f32_i64(float x)
{
    return x;
}
static inline uint8_t fptoui_f32_i8(float x)
{
    return x;
}
static inline uint16_t fptoui_f32_i16(float x)
{
    return x;
}
static inline uint32_t fptoui_f32_i32(float x)
{
    return x;
}
static inline uint64_t fptoui_f32_i64(float x)
{
    return x;
}
static inline float futhark_sin32(float x)
{
    return sin(x);
}
static inline float futhark_cos32(float x)
{
    return cos(x);
}
__kernel void kernel_1137(float y_904, float scale_900, int32_t field_sizze_899,
                          __global unsigned char *mem_1259)
{
    int32_t wave_sizze_1277;
    int32_t group_sizze_1278;
    char thread_active_1279;
    int32_t global_tid_1137;
    int32_t local_tid_1138;
    int32_t gtid_1131;
    int32_t group_id_1139;
    
    global_tid_1137 = get_global_id(0);
    local_tid_1138 = get_local_id(0);
    group_sizze_1278 = get_local_size(0);
    wave_sizze_1277 = LOCKSTEP_WIDTH;
    group_id_1139 = get_group_id(0);
    gtid_1131 = global_tid_1137;
    thread_active_1279 = slt32(gtid_1131, field_sizze_899);
    
    float x_1141;
    float res_1142;
    float res_1143;
    
    if (thread_active_1279) {
        x_1141 = sitofp_i32_f32(gtid_1131);
        res_1142 = x_1141 / y_904;
        res_1143 = res_1142 * scale_900;
    }
    if (thread_active_1279) {
        *(__global float *) &mem_1259[gtid_1131 * 4] = res_1143;
    }
}
__kernel void kernel_1078(float y_904, float scale_900, int32_t degree_901,
                          float res_909, __global unsigned char *mem_1259,
                          int32_t field_sizze_899, __global
                          unsigned char *mem_1262)
{
    int32_t wave_sizze_1280;
    int32_t group_sizze_1281;
    char thread_active_1282;
    int32_t group_id_1080;
    int32_t gtid_1070;
    int32_t global_tid_1078;
    int32_t gtid_1071;
    int32_t local_tid_1079;
    
    global_tid_1078 = get_global_id(0);
    local_tid_1079 = get_local_id(0);
    group_sizze_1281 = get_local_size(0);
    wave_sizze_1280 = LOCKSTEP_WIDTH;
    group_id_1080 = get_group_id(0);
    gtid_1070 = squot32(global_tid_1078, field_sizze_899);
    gtid_1071 = global_tid_1078 - squot32(global_tid_1078, field_sizze_899) *
        field_sizze_899;
    thread_active_1282 = slt32(gtid_1070, field_sizze_899) && slt32(gtid_1071,
                                                                    field_sizze_899);
    
    float res_1084;
    float res_1085;
    float res_1081;
    float x_1083;
    
    if (thread_active_1282) {
        res_1081 = *(__global float *) &mem_1259[gtid_1070 * 4];
        x_1083 = sitofp_i32_f32(gtid_1071);
        res_1084 = x_1083 / y_904;
        res_1085 = res_1084 * scale_900;
    }
    
    float res_1086;
    float binop_param_x_1089 = 0.0F;
    int32_t chunk_sizze_1087;
    int32_t chunk_offset_1088 = 0;
    
    while (slt32(chunk_offset_1088, degree_901)) {
        if (slt32(degree_901 - chunk_offset_1088, degree_901)) {
            chunk_sizze_1087 = degree_901 - chunk_offset_1088;
        } else {
            chunk_sizze_1087 = degree_901;
        }
        
        float res_1091;
        float acc_1094 = binop_param_x_1089;
        int32_t dummy_chunk_sizze_1092;
        int32_t i_1093 = 0;
        
        while (slt32(i_1093, chunk_sizze_1087)) {
            if (slt32(chunk_sizze_1087 - i_1093, 1)) {
                dummy_chunk_sizze_1092 = chunk_sizze_1087 - i_1093;
            } else {
                dummy_chunk_sizze_1092 = 1;
            }
            
            float arg_1104;
            float res_1100;
            float res_1108;
            float res_1101;
            int32_t iota_start_1253;
            float x_1105;
            float x_1098;
            float x_1106;
            float x_1102;
            float arg_1099;
            float res_1107;
            float y_1103;
            
            if (thread_active_1282) {
                iota_start_1253 = i_1093 + chunk_offset_1088;
                x_1098 = sitofp_i32_f32(iota_start_1253);
                arg_1099 = x_1098 * res_909;
                res_1100 = futhark_cos32(arg_1099);
                res_1101 = futhark_sin32(arg_1099);
                x_1102 = res_1100 * res_1085;
                y_1103 = res_1101 * res_1081;
                arg_1104 = x_1102 + y_1103;
                x_1105 = futhark_cos32(arg_1104);
                x_1106 = x_1105 + 1.0F;
                res_1107 = x_1106 / 2.0F;
                res_1108 = acc_1094 + res_1107;
            }
            acc_1094 = res_1108;
            i_1093 += 1;
        }
        res_1091 = acc_1094;
        binop_param_x_1089 = res_1091;
        chunk_offset_1088 += degree_901;
    }
    res_1086 = binop_param_x_1089;
    
    int32_t x_1124;
    int32_t res_1112;
    float y_1120;
    int8_t res_1128;
    float x_1116;
    int32_t res_1113;
    float res_1121;
    int32_t y_1129;
    int32_t tofloat_arg_1109;
    float x_1117;
    int32_t y_1125;
    float y_1110;
    float y_1118;
    int32_t x_1126;
    float x_1114;
    float trunc_arg_1122;
    int32_t res_1130;
    float y_1115;
    int8_t res_1123;
    float res_1111;
    float res_1119;
    float trunc_arg_1127;
    
    if (thread_active_1282) {
        tofloat_arg_1109 = fptosi_f32_i32(res_1086);
        y_1110 = sitofp_i32_f32(tofloat_arg_1109);
        res_1111 = res_1086 - y_1110;
        res_1112 = tofloat_arg_1109 & 1;
        res_1113 = 1 - res_1112;
        x_1114 = sitofp_i32_f32(res_1112);
        y_1115 = 1.0F - res_1111;
        x_1116 = x_1114 * y_1115;
        x_1117 = sitofp_i32_f32(res_1113);
        y_1118 = x_1117 * res_1111;
        res_1119 = x_1116 + y_1118;
        y_1120 = res_1119 * 0.6000000238418579F;
        res_1121 = 0.4000000059604645F + y_1120;
        trunc_arg_1122 = 255.0F * res_1121;
        res_1123 = fptoui_f32_i8(trunc_arg_1122);
        x_1124 = zext_i8_i32(res_1123);
        y_1125 = x_1124 << 8;
        x_1126 = 16711680 | y_1125;
        trunc_arg_1127 = 255.0F * res_1119;
        res_1128 = fptoui_f32_i8(trunc_arg_1127);
        y_1129 = zext_i8_i32(res_1128);
        res_1130 = x_1126 | y_1129;
    }
    if (thread_active_1282) {
        *(__global int32_t *) &mem_1262[(gtid_1070 * field_sizze_899 +
                                         gtid_1071) * 4] = res_1130;
    }
}
__kernel void kernel_1241(int32_t n_steps_976, float time_delta_977, __global
                          unsigned char *mem_1264)
{
    int32_t wave_sizze_1285;
    int32_t group_sizze_1286;
    char thread_active_1287;
    int32_t global_tid_1241;
    int32_t local_tid_1242;
    int32_t gtid_1235;
    int32_t group_id_1243;
    
    global_tid_1241 = get_global_id(0);
    local_tid_1242 = get_local_id(0);
    group_sizze_1286 = get_local_size(0);
    wave_sizze_1285 = LOCKSTEP_WIDTH;
    group_id_1243 = get_group_id(0);
    gtid_1235 = global_tid_1241;
    thread_active_1287 = slt32(gtid_1235, n_steps_976);
    
    float y_1248;
    float res_1249;
    float x_1245;
    float res_1246;
    float res_1250;
    float x_1247;
    
    if (thread_active_1287) {
        x_1245 = sitofp_i32_f32(gtid_1235);
        res_1246 = x_1245 * time_delta_977;
        x_1247 = fpow32(res_1246, 1.5F);
        y_1248 = x_1247 * 4.999999888241291e-3F;
        res_1249 = 1.0F + y_1248;
        res_1250 = 3.1415927410125732F / res_1249;
    }
    if (thread_active_1287) {
        *(__global float *) &mem_1264[gtid_1235 * 4] = res_1250;
    }
}
__kernel void kernel_1228(int32_t n_steps_976, float y_980,
                          int32_t field_sizze_973, float scale_974, __global
                          unsigned char *mem_1267)
{
    int32_t wave_sizze_1288;
    int32_t group_sizze_1289;
    char thread_active_1290;
    int32_t gtid_1220;
    int32_t global_tid_1228;
    int32_t gtid_1221;
    int32_t local_tid_1229;
    int32_t group_id_1230;
    
    global_tid_1228 = get_global_id(0);
    local_tid_1229 = get_local_id(0);
    group_sizze_1289 = get_local_size(0);
    wave_sizze_1288 = LOCKSTEP_WIDTH;
    group_id_1230 = get_group_id(0);
    gtid_1220 = squot32(global_tid_1228, field_sizze_973);
    gtid_1221 = global_tid_1228 - squot32(global_tid_1228, field_sizze_973) *
        field_sizze_973;
    thread_active_1290 = slt32(gtid_1220, n_steps_976) && slt32(gtid_1221,
                                                                field_sizze_973);
    
    float x_1232;
    float res_1233;
    float res_1234;
    
    if (thread_active_1290) {
        x_1232 = sitofp_i32_f32(gtid_1221);
        res_1233 = x_1232 / y_980;
        res_1234 = res_1233 * scale_974;
    }
    if (thread_active_1290) {
        *(__global float *) &mem_1267[(gtid_1220 * field_sizze_973 +
                                       gtid_1221) * 4] = res_1234;
    }
}
__kernel void fut_kernel_map_transpose_f32(__global float *odata,
                                           uint odata_offset, __global
                                           float *idata, uint idata_offset,
                                           uint width, uint height,
                                           uint input_size, uint output_size,
                                           __local float *block)
{
    uint x_index;
    uint y_index;
    uint our_array_offset;
    
    // Adjust the input and output arrays with the basic offset.
    odata += odata_offset / sizeof(float);
    idata += idata_offset / sizeof(float);
    // Adjust the input and output arrays for the third dimension.
    our_array_offset = get_global_id(2) * width * height;
    odata += our_array_offset;
    idata += our_array_offset;
    // read the matrix tile into shared memory
    x_index = get_global_id(0);
    y_index = get_global_id(1);
    
    uint index_in = y_index * width + x_index;
    
    if ((x_index < width && y_index < height) && index_in < input_size)
        block[get_local_id(1) * (FUT_BLOCK_DIM + 1) + get_local_id(0)] =
            idata[index_in];
    barrier(CLK_LOCAL_MEM_FENCE);
    // Write the transposed matrix tile to global memory.
    x_index = get_group_id(1) * FUT_BLOCK_DIM + get_local_id(0);
    y_index = get_group_id(0) * FUT_BLOCK_DIM + get_local_id(1);
    
    uint index_out = y_index * height + x_index;
    
    if ((x_index < height && y_index < width) && index_out < output_size)
        odata[index_out] = block[get_local_id(0) * (FUT_BLOCK_DIM + 1) +
                                 get_local_id(1)];
}
__kernel void kernel_1166(int32_t n_steps_976, __global unsigned char *mem_1264,
                          float y_980, int32_t field_sizze_973, __global
                          unsigned char *mem_1270, float scale_974,
                          int32_t degree_975, __global unsigned char *mem_1274)
{
    int32_t wave_sizze_1291;
    int32_t group_sizze_1292;
    char thread_active_1293;
    int32_t group_id_1168;
    int32_t gtid_1156;
    int32_t gtid_1157;
    int32_t gtid_1158;
    int32_t global_tid_1166;
    int32_t local_tid_1167;
    
    global_tid_1166 = get_global_id(0);
    local_tid_1167 = get_local_id(0);
    group_sizze_1292 = get_local_size(0);
    wave_sizze_1291 = LOCKSTEP_WIDTH;
    group_id_1168 = get_group_id(0);
    gtid_1156 = squot32(global_tid_1166, field_sizze_973 * field_sizze_973);
    gtid_1157 = squot32(global_tid_1166 - squot32(global_tid_1166,
                                                  field_sizze_973 *
                                                  field_sizze_973) *
                        (field_sizze_973 * field_sizze_973), field_sizze_973);
    gtid_1158 = global_tid_1166 - squot32(global_tid_1166, field_sizze_973 *
                                          field_sizze_973) * (field_sizze_973 *
                                                              field_sizze_973) -
        squot32(global_tid_1166 - squot32(global_tid_1166, field_sizze_973 *
                                          field_sizze_973) * (field_sizze_973 *
                                                              field_sizze_973),
                field_sizze_973) * field_sizze_973;
    thread_active_1293 = (slt32(gtid_1156, n_steps_976) && slt32(gtid_1157,
                                                                 field_sizze_973)) &&
        slt32(gtid_1158, field_sizze_973);
    
    float x_1172;
    float res_1173;
    float res_1169;
    float res_1174;
    float res_1170;
    
    if (thread_active_1293) {
        res_1169 = *(__global float *) &mem_1264[gtid_1156 * 4];
        res_1170 = *(__global float *) &mem_1270[(gtid_1157 * n_steps_976 +
                                                  gtid_1156) * 4];
        x_1172 = sitofp_i32_f32(gtid_1158);
        res_1173 = x_1172 / y_980;
        res_1174 = res_1173 * scale_974;
    }
    
    float res_1175;
    float binop_param_x_1178 = 0.0F;
    int32_t chunk_sizze_1176;
    int32_t chunk_offset_1177 = 0;
    
    while (slt32(chunk_offset_1177, degree_975)) {
        if (slt32(degree_975 - chunk_offset_1177, degree_975)) {
            chunk_sizze_1176 = degree_975 - chunk_offset_1177;
        } else {
            chunk_sizze_1176 = degree_975;
        }
        
        float res_1180;
        float acc_1183 = binop_param_x_1178;
        int32_t dummy_chunk_sizze_1181;
        int32_t i_1182 = 0;
        
        while (slt32(i_1182, chunk_sizze_1176)) {
            if (slt32(chunk_sizze_1176 - i_1182, 1)) {
                dummy_chunk_sizze_1181 = chunk_sizze_1176 - i_1182;
            } else {
                dummy_chunk_sizze_1181 = 1;
            }
            
            float arg_1188;
            float res_1196;
            float y_1192;
            float arg_1193;
            float res_1189;
            float res_1197;
            int32_t iota_start_1254;
            float res_1190;
            float x_1194;
            float x_1187;
            float x_1195;
            float x_1191;
            
            if (thread_active_1293) {
                iota_start_1254 = i_1182 + chunk_offset_1177;
                x_1187 = sitofp_i32_f32(iota_start_1254);
                arg_1188 = x_1187 * res_1169;
                res_1189 = futhark_cos32(arg_1188);
                res_1190 = futhark_sin32(arg_1188);
                x_1191 = res_1189 * res_1174;
                y_1192 = res_1190 * res_1170;
                arg_1193 = x_1191 + y_1192;
                x_1194 = futhark_cos32(arg_1193);
                x_1195 = x_1194 + 1.0F;
                res_1196 = x_1195 / 2.0F;
                res_1197 = acc_1183 + res_1196;
            }
            acc_1183 = res_1197;
            i_1182 += 1;
        }
        res_1180 = acc_1183;
        binop_param_x_1178 = res_1180;
        chunk_offset_1177 += degree_975;
    }
    res_1175 = binop_param_x_1178;
    
    float trunc_arg_1216;
    float y_1204;
    int8_t res_1212;
    float res_1200;
    float res_1208;
    int32_t x_1213;
    int32_t res_1201;
    float y_1209;
    int8_t res_1217;
    float x_1205;
    int32_t res_1202;
    float res_1210;
    int32_t y_1218;
    int32_t tofloat_arg_1198;
    float x_1206;
    int32_t y_1214;
    float y_1199;
    float y_1207;
    int32_t x_1215;
    float x_1203;
    float trunc_arg_1211;
    int32_t res_1219;
    
    if (thread_active_1293) {
        tofloat_arg_1198 = fptosi_f32_i32(res_1175);
        y_1199 = sitofp_i32_f32(tofloat_arg_1198);
        res_1200 = res_1175 - y_1199;
        res_1201 = tofloat_arg_1198 & 1;
        res_1202 = 1 - res_1201;
        x_1203 = sitofp_i32_f32(res_1201);
        y_1204 = 1.0F - res_1200;
        x_1205 = x_1203 * y_1204;
        x_1206 = sitofp_i32_f32(res_1202);
        y_1207 = x_1206 * res_1200;
        res_1208 = x_1205 + y_1207;
        y_1209 = res_1208 * 0.6000000238418579F;
        res_1210 = 0.4000000059604645F + y_1209;
        trunc_arg_1211 = 255.0F * res_1210;
        res_1212 = fptoui_f32_i8(trunc_arg_1211);
        x_1213 = zext_i8_i32(res_1212);
        y_1214 = x_1213 << 8;
        x_1215 = 16711680 | y_1214;
        trunc_arg_1216 = 255.0F * res_1208;
        res_1217 = fptoui_f32_i8(trunc_arg_1216);
        y_1218 = zext_i8_i32(res_1217);
        res_1219 = x_1215 | y_1218;
    }
    if (thread_active_1293) {
        *(__global int32_t *) &mem_1274[(gtid_1156 * (field_sizze_973 *
                                                      field_sizze_973) +
                                         gtid_1157 * field_sizze_973 +
                                         gtid_1158) * 4] = res_1219;
    }
}
"""
# Hacky parser/reader for values written in Futhark syntax.  Used for
# reading stdin when compiling standalone programs with the Python
# code generator.

import numpy as np
import string

lookahead_buffer = []

def reset_lookahead():
    global lookahead_buffer
    lookahead_buffer = []

def get_char(f):
    global lookahead_buffer
    if len(lookahead_buffer) == 0:
        return f.read(1)
    else:
        c = lookahead_buffer[0]
        lookahead_buffer = lookahead_buffer[1:]
        return c

def unget_char(f, c):
    global lookahead_buffer
    lookahead_buffer = [c] + lookahead_buffer

def peek_char(f):
    c = get_char(f)
    if c:
        unget_char(f, c)
    return c

def skip_spaces(f):
    c = get_char(f)
    while c != None:
        if c.isspace():
            c = get_char(f)
        elif c == '-':
          # May be line comment.
          if peek_char(f) == '-':
            # Yes, line comment. Skip to end of line.
            while (c != '\n' and c != None):
              c = get_char(f)
          else:
            break
        else:
          break
    if c:
        unget_char(f, c)

def parse_specific_char(f, expected):
    got = get_char(f)
    if got != expected:
        unget_char(f, got)
        raise ValueError
    return True

def parse_specific_string(f, s):
    for c in s:
        parse_specific_char(f, c)
    return True

def optional(p, *args):
    try:
        return p(*args)
    except ValueError:
        return None

def sepBy(p, sep, *args):
    elems = []
    x = optional(p, *args)
    if x != None:
        elems += [x]
        while optional(sep, *args) != None:
            x = p(*args)
            elems += [x]
    return elems

def parse_int(f):
    s = ''
    c = get_char(f)
    if c == '0' and peek_char(f) in ['x', 'X']:
        c = get_char(f) # skip X
        c = get_char(f)
        while c != None:
            if c in string.hexdigits:
                s += c
                c = get_char(f)
            else:
                unget_char(f, c)
                s = str(int(s, 16))
                break
    else:
        while c != None:
            if c.isdigit():
                s += c
                c = get_char(f)
            else:
                unget_char(f, c)
                break
    optional(read_int_trailer, f)
    return s

def parse_int_signed(f):
    s = ''
    c = get_char(f)

    if c == '-' and peek_char(f).isdigit():
      s = c + parse_int(f)
    else:
      if c <> '+':
          unget_char(f, c)
      s = parse_int(f)

    return s

def read_int_trailer(f):
  parse_specific_char(f, 'i')
  while peek_char(f).isdigit():
    get_char(f)

def read_comma(f):
    skip_spaces(f)
    parse_specific_char(f, ',')
    return ','

def read_int(f):
    skip_spaces(f)
    return int(parse_int_signed(f))

def read_char(f):
    skip_spaces(f)
    parse_specific_char(f, '\'')
    c = get_char(f)
    parse_specific_char(f, '\'')
    return c

def read_double(f):
    skip_spaces(f)
    c = get_char(f)
    if (c == '-'):
      sign = '-'
    else:
      unget_char(f,c)
      sign = ''
    bef = optional(parse_int, f)
    if bef == None:
        bef = '0'
        parse_specific_char(f, '.')
        aft = parse_int(f)
    elif optional(parse_specific_char, f, '.'):
        aft = parse_int(f)
    else:
        aft = '0'
    if (optional(parse_specific_char, f, 'E') or
        optional(parse_specific_char, f, 'e')):
        expt = parse_int_signed(f)
    else:
        expt = '0'
    optional(read_float_trailer, f)
    return float(sign + bef + '.' + aft + 'E' + expt)

def read_float(f):
    return read_double(f)

def read_float_trailer(f):
  parse_specific_char(f, 'f')
  while peek_char(f).isdigit():
    get_char(f)

def read_bool(f):
    skip_spaces(f)
    if peek_char(f) == 'T':
        parse_specific_string(f, 'True')
        return True
    elif peek_char(f) == 'F':
        parse_specific_string(f, 'False')
        return False
    else:
        raise ValueError

def read_empty_array(f, type_name, rank):
    parse_specific_string(f, 'empty')
    parse_specific_char(f, '(')
    for i in range(rank):
        parse_specific_string(f, '[]')
    parse_specific_string(f, type_name)
    parse_specific_char(f, ')')
    return []

def read_array_elems(f, elem_reader, type_name, rank):
    skip_spaces(f)
    try:
        parse_specific_char(f, '[')
    except ValueError:
        return read_empty_array(f, type_name, rank)
    else:
        xs = sepBy(elem_reader, read_comma, f)
        skip_spaces(f)
        parse_specific_char(f, ']')
        return xs

def read_array_helper(f, elem_reader, type_name, rank):
    def nested_row_reader(_):
        return read_array_helper(f, elem_reader, type_name, rank-1)
    if rank == 1:
        row_reader = elem_reader
    else:
        row_reader = nested_row_reader
    return read_array_elems(f, row_reader, type_name, rank-1)

def expected_array_dims(l, rank):
  if rank > 1:
      n = len(l)
      if n == 0:
          elem = []
      else:
          elem = l[0]
      return [n] + expected_array_dims(elem, rank-1)
  else:
      return [len(l)]

def verify_array_dims(l, dims):
    if dims[0] != len(l):
        raise ValueError
    if len(dims) > 1:
        for x in l:
            verify_array_dims(x, dims[1:])

def read_double_signed(f):

    skip_spaces(f)
    c = get_char(f)

    if c == '-' and peek_char(f).isdigit():
      v = -1 * read_double(f)
    else:
      unget_char(f, c)
      v = read_double(f)

    return v

def read_array(f, elem_reader, type_name, rank, bt):
    elems = read_array_helper(f, elem_reader, type_name, rank)
    dims = expected_array_dims(elems, rank)
    verify_array_dims(elems, dims)
    return np.array(elems, dtype=bt)
# Scalar functions.

import numpy as np

def signed(x):
  if type(x) == np.uint8:
    return np.int8(x)
  elif type(x) == np.uint16:
    return np.int16(x)
  elif type(x) == np.uint32:
    return np.int32(x)
  else:
    return np.int64(x)

def unsigned(x):
  if type(x) == np.int8:
    return np.uint8(x)
  elif type(x) == np.int16:
    return np.uint16(x)
  elif type(x) == np.int32:
    return np.uint32(x)
  else:
    return np.uint64(x)

def shlN(x,y):
  return x << y

def ashrN(x,y):
  return x >> y

def sdivN(x,y):
  return x / y

def smodN(x,y):
  return x % y

def udivN(x,y):
  return signed(unsigned(x) / unsigned(y))

def umodN(x,y):
  return signed(unsigned(x) % unsigned(y))

def squotN(x,y):
  return np.int32(float(x) / float(y))

def sremN(x,y):
  return np.fmod(x,y)

def powN(x,y):
  return x ** y

def fpowN(x,y):
  return x ** y

def sleN(x,y):
  return x <= y

def sltN(x,y):
  return x < y

def uleN(x,y):
  return unsigned(x) <= unsigned(y)

def ultN(x,y):
  return unsigned(x) < unsigned(y)

def lshr8(x,y):
  return np.int8(np.uint8(x) >> np.uint8(y))

def lshr16(x,y):
  return np.int16(np.uint16(x) >> np.uint16(y))

def lshr32(x,y):
  return np.int32(np.uint32(x) >> np.uint32(y))

def lshr64(x,y):
  return np.int64(np.uint64(x) >> np.uint64(y))

def sext_T_i8(x):
  return np.int8(x)

def sext_T_i16(x):
  return np.int16(x)

def sext_T_i32(x):
  return np.int32(x)

def sext_T_i64(x):
  return np.int32(x)

def zext_i8_i8(x):
  return np.int8(np.uint8(x))

def zext_i8_i16(x):
  return np.int16(np.uint8(x))

def zext_i8_i32(x):
  return np.int32(np.uint8(x))

def zext_i8_i64(x):
  return np.int64(np.uint8(x))

def zext_i16_i8(x):
  return np.int8(np.uint16(x))

def zext_i16_i16(x):
  return np.int16(np.uint16(x))

def zext_i16_i32(x):
  return np.int32(np.uint16(x))

def zext_i16_i64(x):
  return np.int64(np.uint16(x))

def zext_i32_i8(x):
  return np.int8(np.uint32(x))

def zext_i32_i16(x):
  return np.int16(np.uint32(x))

def zext_i32_i32(x):
  return np.int32(np.uint32(x))

def zext_i32_i64(x):
  return np.int64(np.uint32(x))

def zext_i64_i8(x):
  return np.int8(np.uint64(x))

def zext_i64_i16(x):
  return np.int16(np.uint64(x))

def zext_i64_i32(x):
  return np.int32(np.uint64(x))

def zext_i64_i64(x):
  return np.int64(np.uint64(x))

shl8 = shl16 = shl32 = shl64 = shlN
ashr8 = ashr16 = ashr32 = ashr64 = ashrN
sdiv8 = sdiv16 = sdiv32 = sdiv64 = sdivN
smod8 = smod16 = smod32 = smod64 = smodN
udiv8 = udiv16 = udiv32 = udiv64 = udivN
umod8 = umod16 = umod32 = umod64 = umodN
squot8 = squot16 = squot32 = squot64 = squotN
srem8 = srem16 = srem32 = srem64 = sremN
pow8 = pow16 = pow32 = pow64 = powN
fpow32 = fpow64 = fpowN
sle8 = sle16 = sle32 = sle64 = sleN
slt8 = slt16 = slt32 = slt64 = sltN
ule8 = ule16 = ule32 = ule64 = uleN
ult8 = ult16 = ult32 = ult64 = ultN
sext_i8_i8 = sext_i16_i8 = sext_i32_i8 = sext_i64_i8 = sext_T_i8
sext_i8_i16 = sext_i16_i16 = sext_i32_i16 = sext_i64_i16 = sext_T_i16
sext_i8_i32 = sext_i16_i32 = sext_i32_i32 = sext_i64_i32 = sext_T_i32
sext_i8_i64 = sext_i16_i64 = sext_i32_i64 = sext_i64_i64 = sext_T_i64

def ssignum(x):
  return np.sign(x)

def usignum(x):
  if x < 0:
    return ssignum(-x)
  else:
    return ssignum(x)

def sitofp_T_f32(x):
  return np.float32(x)
sitofp_i8_f32 = sitofp_i16_f32 = sitofp_i32_f32 = sitofp_i64_f32 = sitofp_T_f32

def sitofp_T_f64(x):
  return np.float64(x)
sitofp_i8_f64 = sitofp_i16_f64 = sitofp_i32_f64 = sitofp_i64_f64 = sitofp_T_f64

def uitofp_T_f32(x):
  return np.float32(unsigned(x))
uitofp_i8_f32 = uitofp_i16_f32 = uitofp_i32_f32 = uitofp_i64_f32 = uitofp_T_f32

def uitofp_T_f64(x):
  return np.float64(unsigned(x))
uitofp_i8_f64 = uitofp_i16_f64 = uitofp_i32_f64 = uitofp_i64_f64 = uitofp_T_f64

def fptosi_T_i8(x):
  return np.int8(np.trunc(x))
fptosi_f32_i8 = fptosi_f64_i8 = fptosi_T_i8

def fptosi_T_i16(x):
  return np.int16(np.trunc(x))
fptosi_f32_i16 = fptosi_f64_i16 = fptosi_T_i16

def fptosi_T_i32(x):
  return np.int32(np.trunc(x))
fptosi_f32_i32 = fptosi_f64_i32 = fptosi_T_i32

def fptosi_T_i64(x):
  return np.int64(np.trunc(x))
fptosi_f32_i64 = fptosi_f64_i64 = fptosi_T_i64

def fptoui_T_i8(x):
  return np.uint8(np.trunc(x))
fptoui_f32_i8 = fptoui_f64_i8 = fptoui_T_i8

def fptoui_T_i16(x):
  return np.uint16(np.trunc(x))
fptoui_f32_i16 = fptoui_f64_i16 = fptoui_T_i16

def fptoui_T_i32(x):
  return np.uint32(np.trunc(x))
fptoui_f32_i32 = fptoui_f64_i32 = fptoui_T_i32

def fptoui_T_i64(x):
  return np.uint64(np.trunc(x))
fptoui_f32_i64 = fptoui_f64_i64 = fptoui_T_i64

def fpconv_f32_f64(x):
  return np.float64(x)

def fpconv_f64_f32(x):
  return np.float32(x)

def futhark_log64(x):
  return np.float64(np.log(x))

def futhark_sqrt64(x):
  return np.sqrt(x)

def futhark_exp64(x):
  return np.exp(x)

def futhark_cos64(x):
  return np.cos(x)

def futhark_sin64(x):
  return np.sin(x)

def futhark_atan2_64(x, y):
  return np.arctan2(x, y)

def futhark_isnan64(x):
  return np.isnan(x)

def futhark_isinf64(x):
  return np.isinf(x)

def futhark_log32(x):
  return np.float32(np.log(x))

def futhark_sqrt32(x):
  return np.float32(np.sqrt(x))

def futhark_exp32(x):
  return np.exp(x)

def futhark_cos32(x):
  return np.cos(x)

def futhark_sin32(x):
  return np.sin(x)

def futhark_atan2_32(x, y):
  return np.arctan2(x, y)

def futhark_isnan32(x):
  return np.isnan(x)

def futhark_isinf32(x):
  return np.isinf(x)
class crystal:
  def __init__(self, interactive=False, platform_pref=preferred_platform,
               device_pref=preferred_device):
    self.ctx = get_prefered_context(interactive, platform_pref, device_pref)
    self.queue = cl.CommandQueue(self.ctx)
     # XXX: Assuming just a single device here.
    platform_name = self.ctx.get_info(cl.context_info.DEVICES)[0].platform.name
    device_type = self.ctx.get_info(cl.context_info.DEVICES)[0].type
    lockstep_width = 1
    if ((platform_name == "NVIDIA CUDA") and (device_type == cl.device_type.GPU)):
      lockstep_width = np.int32(32)
    if ((platform_name == "AMD Accelerated Parallel Processing") and (device_type == cl.device_type.GPU)):
      lockstep_width = np.int32(64)
    if (len(fut_opencl_src) >= 0):
      program = cl.Program(self.ctx, fut_opencl_src).build(["-DFUT_BLOCK_DIM={}".format(FUT_BLOCK_DIM), "-DLOCKSTEP_WIDTH={}".format(lockstep_width)])
    
    self.kernel_1137_var = program.kernel_1137
    self.kernel_1078_var = program.kernel_1078
    self.kernel_1241_var = program.kernel_1241
    self.kernel_1228_var = program.kernel_1228
    self.fut_kernel_map_transpose_f32_var = program.fut_kernel_map_transpose_f32
    self.kernel_1166_var = program.kernel_1166
  def futhark_render_frame(self, field_size_899, scale_900, degree_901,
                           time_902):
    y_904 = sitofp_i32_f32(field_size_899)
    x_906 = fpow32(time_902, np.float32((1.5)))
    y_907 = (x_906 * np.float32((5.0e-3)))
    res_908 = (np.float32((1.0)) + y_907)
    res_909 = (np.float32((3.1415927)) / res_908)
    group_size_1132 = cl_group_size
    y_1133 = (group_size_1132 - np.int32(1))
    x_1134 = (field_size_899 + y_1133)
    num_groups_1135 = squot32(x_1134, group_size_1132)
    num_threads_1136 = (num_groups_1135 * group_size_1132)
    bytes_1258 = (np.int32(4) * field_size_899)
    mem_1259 = cl.Buffer(self.ctx, cl.mem_flags.READ_WRITE,
                         long(long(bytes_1258) if (bytes_1258 > np.int32(0)) else np.int32(1)))
    if ((np.int32(1) * (num_groups_1135 * group_size_1132)) != np.int32(0)):
      self.kernel_1137_var.set_args(np.float32(y_904), np.float32(scale_900),
                                    np.int32(field_size_899), mem_1259)
      cl.enqueue_nd_range_kernel(self.queue, self.kernel_1137_var,
                                 (long((num_groups_1135 * group_size_1132)),),
                                 (long(group_size_1132),))
      if synchronous:
        self.queue.finish()
    nesting_size_1072 = (field_size_899 * field_size_899)
    x_1075 = (nesting_size_1072 + y_1133)
    num_groups_1076 = squot32(x_1075, group_size_1132)
    num_threads_1077 = (num_groups_1076 * group_size_1132)
    bytes_1260 = (bytes_1258 * field_size_899)
    mem_1262 = cl.Buffer(self.ctx, cl.mem_flags.READ_WRITE,
                         long(long(bytes_1260) if (bytes_1260 > np.int32(0)) else np.int32(1)))
    if ((np.int32(1) * (num_groups_1076 * group_size_1132)) != np.int32(0)):
      self.kernel_1078_var.set_args(np.float32(y_904), np.float32(scale_900),
                                    np.int32(degree_901), np.float32(res_909),
                                    mem_1259, np.int32(field_size_899),
                                    mem_1262)
      cl.enqueue_nd_range_kernel(self.queue, self.kernel_1078_var,
                                 (long((num_groups_1076 * group_size_1132)),),
                                 (long(group_size_1132),))
      if synchronous:
        self.queue.finish()
    out_mem_1275 = mem_1262
    out_memsize_1276 = bytes_1260
    return (out_memsize_1276, out_mem_1275)
  def futhark_main(self, field_size_973, scale_974, degree_975, n_steps_976,
                   time_delta_977):
    y_980 = sitofp_i32_f32(field_size_973)
    group_size_1236 = cl_group_size
    y_1237 = (group_size_1236 - np.int32(1))
    x_1238 = (n_steps_976 + y_1237)
    num_groups_1239 = squot32(x_1238, group_size_1236)
    num_threads_1240 = (num_groups_1239 * group_size_1236)
    bytes_1263 = (np.int32(4) * n_steps_976)
    mem_1264 = cl.Buffer(self.ctx, cl.mem_flags.READ_WRITE,
                         long(long(bytes_1263) if (bytes_1263 > np.int32(0)) else np.int32(1)))
    if ((np.int32(1) * (num_groups_1239 * group_size_1236)) != np.int32(0)):
      self.kernel_1241_var.set_args(np.int32(n_steps_976),
                                    np.float32(time_delta_977), mem_1264)
      cl.enqueue_nd_range_kernel(self.queue, self.kernel_1241_var,
                                 (long((num_groups_1239 * group_size_1236)),),
                                 (long(group_size_1236),))
      if synchronous:
        self.queue.finish()
    nesting_size_1222 = (field_size_973 * n_steps_976)
    x_1225 = (nesting_size_1222 + y_1237)
    num_groups_1226 = squot32(x_1225, group_size_1236)
    num_threads_1227 = (num_groups_1226 * group_size_1236)
    bytes_1265 = (bytes_1263 * field_size_973)
    mem_1267 = cl.Buffer(self.ctx, cl.mem_flags.READ_WRITE,
                         long(long(bytes_1265) if (bytes_1265 > np.int32(0)) else np.int32(1)))
    if ((np.int32(1) * (num_groups_1226 * group_size_1236)) != np.int32(0)):
      self.kernel_1228_var.set_args(np.int32(n_steps_976), np.float32(y_980),
                                    np.int32(field_size_973),
                                    np.float32(scale_974), mem_1267)
      cl.enqueue_nd_range_kernel(self.queue, self.kernel_1228_var,
                                 (long((num_groups_1226 * group_size_1236)),),
                                 (long(group_size_1236),))
      if synchronous:
        self.queue.finish()
    nesting_size_1159 = (field_size_973 * field_size_973)
    nesting_size_1160 = (nesting_size_1159 * n_steps_976)
    x_1163 = (nesting_size_1160 + y_1237)
    num_groups_1164 = squot32(x_1163, group_size_1236)
    num_threads_1165 = (num_groups_1164 * group_size_1236)
    binop_x_1269 = (np.int32(4) * field_size_973)
    bytes_1268 = (binop_x_1269 * n_steps_976)
    mem_1270 = cl.Buffer(self.ctx, cl.mem_flags.READ_WRITE,
                         long(long(bytes_1268) if (bytes_1268 > np.int32(0)) else np.int32(1)))
    if ((((np.int32(1) * (field_size_973 + srem32((np.int32(16) - srem32(field_size_973,
                                                                         np.int64(16))),
                                                  np.int64(16)))) * (n_steps_976 + srem32((np.int32(16) - srem32(n_steps_976,
                                                                                                                 np.int64(16))),
                                                                                          np.int64(16)))) * np.int64(1)) != np.int32(0)):
      self.fut_kernel_map_transpose_f32_var.set_args(mem_1270,
                                                     np.int32(np.int64(0)),
                                                     mem_1267,
                                                     np.int32(np.int64(0)),
                                                     np.int32(field_size_973),
                                                     np.int32(n_steps_976),
                                                     np.int32((field_size_973 * n_steps_976)),
                                                     np.int32((field_size_973 * n_steps_976)),
                                                     cl.LocalMemory(long((np.int32(272) * np.int32(4)))))
      cl.enqueue_nd_range_kernel(self.queue,
                                 self.fut_kernel_map_transpose_f32_var,
                                 (long((field_size_973 + srem32((np.int32(16) - srem32(field_size_973,
                                                                                       np.int64(16))),
                                                                np.int64(16)))),
                                  long((n_steps_976 + srem32((np.int32(16) - srem32(n_steps_976,
                                                                                    np.int64(16))),
                                                             np.int64(16)))),
                                  long(np.int64(1))), (long(np.int64(16)),
                                                       long(np.int64(16)),
                                                       long(np.int64(1))))
      if synchronous:
        self.queue.finish()
    bytes_1271 = (bytes_1265 * field_size_973)
    mem_1274 = cl.Buffer(self.ctx, cl.mem_flags.READ_WRITE,
                         long(long(bytes_1271) if (bytes_1271 > np.int32(0)) else np.int32(1)))
    if ((np.int32(1) * (num_groups_1164 * group_size_1236)) != np.int32(0)):
      self.kernel_1166_var.set_args(np.int32(n_steps_976), mem_1264,
                                    np.float32(y_980), np.int32(field_size_973),
                                    mem_1270, np.float32(scale_974),
                                    np.int32(degree_975), mem_1274)
      cl.enqueue_nd_range_kernel(self.queue, self.kernel_1166_var,
                                 (long((num_groups_1164 * group_size_1236)),),
                                 (long(group_size_1236),))
      if synchronous:
        self.queue.finish()
    out_mem_1283 = mem_1274
    out_memsize_1284 = bytes_1271
    return (out_memsize_1284, out_mem_1283)
  def render_frame(self, field_size_899_ext, scale_900_ext, degree_901_ext,
                   time_902_ext):
    field_size_899 = np.int32(field_size_899_ext)
    scale_900 = np.float32(scale_900_ext)
    degree_901 = np.int32(degree_901_ext)
    time_902 = np.float32(time_902_ext)
    (out_memsize_1276, out_mem_1275) = self.futhark_render_frame(field_size_899,
                                                                 scale_900,
                                                                 degree_901,
                                                                 time_902)
    return cl.array.Array(self.queue, (field_size_899, field_size_899),
                          ct.c_int32, data=out_mem_1275)
  def main(self, field_size_973_ext, scale_974_ext, degree_975_ext,
           n_steps_976_ext, time_delta_977_ext):
    field_size_973 = np.int32(field_size_973_ext)
    scale_974 = np.float32(scale_974_ext)
    degree_975 = np.int32(degree_975_ext)
    n_steps_976 = np.int32(n_steps_976_ext)
    time_delta_977 = np.float32(time_delta_977_ext)
    (out_memsize_1284, out_mem_1283) = self.futhark_main(field_size_973,
                                                         scale_974, degree_975,
                                                         n_steps_976,
                                                         time_delta_977)
    return cl.array.Array(self.queue, (n_steps_976, field_size_973,
                                       field_size_973), ct.c_int32,
                          data=out_mem_1283)
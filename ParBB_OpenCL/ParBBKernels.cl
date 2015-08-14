// Segmented Reduce, Scan, Filter, etc
#ifndef ParBB_OpenCL
#define ParBB_OpenCL
 
typedef float ftk_float;
typedef /*ulong*/uint ftk_ulong;
typedef uchar ftk_uchar; 

#include "DataStructs.h"

/////////////////////////////////////
/// Parallel Basic Blocks Helpers ///
/////////////////////////////////////

// store to fast memory
inline void storeMyRec( MyRec* src, uint ind
                      , volatile __local ftk_uchar* cacheC
                      , volatile __local ftk_ulong* cacheL
                      , volatile __local ftk_float* cacheF
) {
    cacheC[ind] = src->c;
    cacheL[ind] = src->i;
    cacheF[ind] = src->f;
}
 
// load from fast memory
inline void loadMyRec ( MyRec* dst, uint ind
                      , volatile __local ftk_uchar* cacheC
                      , volatile __local ftk_ulong* cacheL
                      , volatile __local ftk_float* cacheF
) {
    dst->c = cacheC[ind];
    dst->i = cacheL[ind];
    dst->f = cacheF[ind];
}    

// shorthand for reading, applying the associative operator
// and writing back to fast memory
inline void applyMyRecSHM(  MyRec* dst, MyRec* src1, MyRec* src2  
                         ,  uint ind,    uint ofs 
                         ,  volatile __local ftk_uchar* cacheC
                         ,  volatile __local ftk_ulong* cacheL
                         ,  volatile __local ftk_float* cacheF
) {
    loadMyRec(src1, ind-ofs, cacheC, cacheL, cacheF);
    loadMyRec(src2, ind,     cacheC, cacheL, cacheF);
    applyMyRecOP(dst, src1,  src2 );
    storeMyRec( dst, ind, cacheC, cacheL, cacheF );
}

//////////////////////////////////////////
// Warp and Block Level Inclusive Scans //
//////////////////////////////////////////
// Warp-Level
inline void scanIncWarpMyRec( MyRec* dst, MyRec* src1, MyRec* src2, const uint idx 
                            , volatile __local ftk_uchar* cacheC
                            , volatile __local ftk_ulong* cacheL
                            , volatile __local ftk_float* cacheF
) {
    const uint lane = idx  & (WARP-1);

    if( lane >= 1 ) applyMyRecSHM( dst, src1, src2, idx, 1, cacheC, cacheL, cacheF );
    if( lane >= 2 ) applyMyRecSHM( dst, src1, src2, idx, 2, cacheC, cacheL, cacheF );
    if( lane >= 4 ) applyMyRecSHM( dst, src1, src2, idx, 4, cacheC, cacheL, cacheF );
    if( lane >= 8 ) applyMyRecSHM( dst, src1, src2, idx, 8, cacheC, cacheL, cacheF );
#if (WARP == 32)
    if( lane >= 16) applyMyRecSHM( dst, src1, src2, idx,16, cacheC, cacheL, cacheF );
#endif
}

// Block-Level
inline void scanIncBlockMyRec(  MyRec* dst, MyRec* src, const uint idx
                             ,  volatile __local ftk_uchar* cacheC
                             ,  volatile __local ftk_ulong* cacheL
                             ,  volatile __local ftk_float* cacheF
) {
    const uint lane   = idx &  (WARP-1);
    const uint warpid = idx >> lgWARP;
    MyRec tmp1, tmp2; 

    // Assumes data already in `src' => copy `src' to shared memory
    storeMyRec( src, idx,  cacheC, cacheL, cacheF);

    // warp-level result in src
    scanIncWarpMyRec(src, &tmp1, &tmp2, idx, cacheC, cacheL, cacheF);
    barrier( CLK_LOCAL_MEM_FENCE );

    // Place the end-of-warp results in the first warp. 
    // For CUDA works: warp size = 32, and max block size = 32^2 = 1024!
    // For AMD, WARP==16 => requires block size to be \leq 16^2 = 256!
    if (lane == WARP-1) { 
        storeMyRec( src, warpid, cacheC, cacheL, cacheF );
    }
    barrier( CLK_LOCAL_MEM_FENCE );

    if (warpid == 0) {
        scanIncWarpMyRec(dst, &tmp1, &tmp2, idx, cacheC, cacheL, cacheF);
    }
    barrier( CLK_LOCAL_MEM_FENCE );

    if (warpid > 0) loadMyRec  (&tmp1, warpid-1, cacheC, cacheL, cacheF);
    else            loadNeutral(&tmp1);
    applyMyRecOP(dst, &tmp1, src);
}



// Block-Level
inline void sgmScanIncBlockMyRec
            (  MyRec* dst, MyRec* src, const uint idx
            ,  volatile __local ftk_uchar* cacheC
            ,  volatile __local ftk_ulong* cacheL
            ,  volatile __local ftk_float* cacheF
) {
    const uint lane   = idx &  (WARP-1);
    const uint warpid = idx >> lgWARP;
    MyRec tmp1, tmp2; 

    // Assumes data already in `src' => copy `src' to shared memory
    storeMyRec( src, idx,  cacheC, cacheL, cacheF);

    // 1a: record whether this warp begins with an ``open'' segment.
    bool warp_is_open = (cacheC[(warpid << 5)] == 0);

    // 1b: intra-warp segmented scan for each warp
    //     warp-level result in src
    scanIncWarpMyRec(src, &tmp1, &tmp2, idx, cacheC, cacheL, cacheF);

    // 2a: warp_flag is the OR-reduction of the flags 
    //     in a warp, and is computed indirectly from
    //     the mindex in hd[]
    bool will_accum= warp_is_open && src->c == 0; //(cacheC[idx] == 0);
    barrier( CLK_LOCAL_MEM_FENCE );

    // 2b  Place the end-of-warp results in the first warp. 
    //     For CUDA works: warp size = 32, and max block size = 32^2 = 1024!
    //     For AMD, WARP==16 => requires block size to be \leq 16^2 = 256!
    if (lane == WARP-1) {
        bool warp_flag = (src->c!=0) || (!warp_is_open);
        cacheC[warpid] = warp_flag;
        cacheL[warpid] = src->i;
        cacheF[warpid] = src->f;
    }
    barrier( CLK_LOCAL_MEM_FENCE );

    // 2c scan the first warp 
    if (warpid == 0) {
        scanIncWarpMyRec(dst, &tmp1, &tmp2, idx, cacheC, cacheL, cacheF);
    }
    barrier( CLK_LOCAL_MEM_FENCE );

    // 3  Finally, add the inter-warp scan results to each element.
    if (warpid > 0 && will_accum) { 
         loadMyRec  (&tmp1, warpid-1, cacheC, cacheL, cacheF);
    } else loadNeutral(&tmp1);

    applyMyRecOP(dst, &tmp1, src);
}
///////////////////////////////
// Inclusive Scan Kernels:
///////////////////////////////
__kernel void scanIncBlockMyRecKer(
        __global ftk_uchar* odata1,
        __global ftk_ulong* odata2,
        __global ftk_float* odata3,
        __global ftk_uchar* idata1,
        __global ftk_ulong* idata2,
        __global ftk_float* idata3,
        uint                arr_len,
        volatile __local  ftk_uchar* cacheC,
        volatile __local  ftk_ulong* cacheL,
        volatile __local  ftk_float* cacheF
) {
    MyRec dst, src;
    uint lid = get_local_id(0);
    uint gid = get_global_id(0);

    if(gid < arr_len) {
        src.c = idata1[gid];
        src.i = idata2[gid];
        src.f = idata3[gid];
    } else {
        loadNeutral(&src);
    }

    scanIncBlockMyRec(&dst, &src, lid, cacheC, cacheL, cacheF);

    if (gid < arr_len) {
        odata1[gid] = dst.c; odata2[gid] = dst.i; odata3[gid] = dst.f;
    }
}

__kernel void scanIncShrinkMyRecKer(
        __global ftk_uchar* odata1, 
        __global ftk_ulong* odata2,
        __global ftk_float* odata3,  

        __global ftk_uchar* blksum1, 
        __global ftk_ulong* blksum2,
        __global ftk_float* blksum3,  

        __global ftk_uchar* idata1,
        __global ftk_ulong* idata2,
        __global ftk_float* idata3, 
        uint                seq_chunk, 
//        uint                par_deg,
        uint                arr_len,

        volatile __local  ftk_uchar* cacheC,
        volatile __local  ftk_ulong* cacheL,
        volatile __local  ftk_float* cacheF
) { 
    const uint gid = get_global_id(0); 
    const uint glen= get_global_size(0);
    const uint lid = get_local_id(0);
    uint i   = 0;
    MyRec dst, src; 
    loadNeutral(&src);

    uint ind = gid;
    for(i=0; i<seq_chunk; i++) {
        MyRec tmp;
        dst.c = idata1[ind]; dst.i = idata2[ind]; dst.f = idata3[ind];
        tmp.c = src.c;       tmp.i = src.i;       tmp.f = src.f;
        applyMyRecOP(&src, &tmp, &dst);
        ind += glen; 
    }

    // block scan
    scanIncBlockMyRec(&dst, &src, lid, cacheC, cacheL, cacheF);

    { // write the block summary, i.e., one elem per thread
        blksum1[gid] = dst.c;
        blksum2[gid] = dst.i;
        blksum3[gid] = dst.f;
    }

    // save the scanned summary of a block
    if ( lid == (get_local_size(0)-1) ) { // write an element per block!
        uint bind = gid / get_local_size(0);
        odata1[bind] = dst.c; 
        odata2[bind] = dst.i; 
        odata3[bind] = dst.f;
    }
}


__kernel void scanIncExpandMyRecKer(
        __global ftk_uchar* odata1, 
        __global ftk_ulong* odata2,
        __global ftk_float* odata3,

        __global ftk_uchar* tmp1,
        __global ftk_ulong* tmp2,
        __global ftk_float* tmp3, 

        __global ftk_uchar* blksum1, 
        __global ftk_ulong* blksum2,
        __global ftk_float* blksum3,  

        uint                seq_chunk, 
//        uint                par_deg,
        uint                arr_len

//        __local  ftk_uchar* cacheC,
//        __local  ftk_ulong* cacheL,
//        __local  ftk_float* cacheF
) { 
    uint gid = get_global_id(0);
    uint glen= get_global_size(0);
    int group_id = gid / get_local_size(0);
    MyRec dst, src1, src2;

    if( get_local_id(0) > 0 ) {
        src2.c = blksum1[gid-1];
        src2.i = blksum2[gid-1]; 
        src2.f = blksum3[gid-1];
    } else {
        loadNeutral(&src2);
    }

    if ( group_id > 0  ) {
        src1.c = tmp1[group_id-1];
        src1.i = tmp2[group_id-1];
        src1.f = tmp3[group_id-1]; 
    } else {
        loadNeutral(&src1);
    }

    applyMyRecOP(&dst, &src1, &src2); 

    uint ind = gid;
    uint i = 0;
    for(i=0; i<seq_chunk; i++) {
        src2.c = odata1[ind]; src2.i = odata2[ind]; src2.f = odata3[ind];
        src1.c = dst.c;       src1.i = dst.i;       src1.f = dst.f;

        applyMyRecOP(&dst, &src1, &src2);

        odata1[ind] = dst.c;  odata2[ind] = dst.i;  odata3[ind] = dst.f;
        ind += glen;
    }
}


__kernel void smallSgmIncScanMyRecKer(
        __global ftk_ulong* odata1,
        __global ftk_float* odata2,
        __global ftk_ulong* idata1,
        __global ftk_float* idata2,
        uint                arr_len,
        uint                sgm_len,
        uint                iddle,
        volatile __local ftk_uchar* cacheC,
        volatile __local ftk_ulong* cacheL,
        volatile __local ftk_float* cacheF
) {
    MyRec dst, src;
    uint lid   = get_local_id(0);
    uint gid   = get_group_id(0)*(get_local_size(0)-iddle) + lid;
    bool valid = (lid < get_local_size(0) - iddle) && (gid < arr_len);
    if(valid) {
        uint r = gid % sgm_len;
        src.c = (r == 0) ? 1 : 0;
        src.i = idata1[gid];
        src.f = idata2[gid];
    } else {
        loadNeutral(&src);
    }

    sgmScanIncBlockMyRec(&dst, &src, lid, cacheC, cacheL, cacheF);

    if (valid) {
        odata1[gid] = dst.i; 
        odata2[gid] = dst.f;
    }
}

///////////////////////////////////////////////////
/// Padded Transposition Kernels
///////////////////////////////////////////////////
__kernel __attribute__((reqd_work_group_size(TILE, TILE, 1)))
void transposeToPad(
        __global ftk_uchar* odata1, 
        __global ftk_ulong* odata2,
        __global ftk_float* odata3,  
        __global ftk_uchar* idata1,
        __global ftk_ulong* idata2,
        __global ftk_float* idata3,
        uint                width, 
        uint                height,
        uint                orig_size 
        // For some reason it does not seem to work
        // if one allocates ONE sh mem and uses pointer
        // arithmetic and casts to partition it into 3 parts!
//      , __local char*  shmem_char,
//        __local ulong* shmem_ulong,
//        __local float* shmem_float
) {
    uint xIndex, yIndex;
    __local ftk_uchar shmem_uchar[TILE*TILE+TILE];
    __local ftk_ulong shmem_ulong[TILE*TILE+TILE];
    __local ftk_float shmem_float[TILE*TILE+TILE];

    // read the matrix tile into shared memory
    xIndex = get_global_id(0);
    yIndex = get_global_id(1); 

    if((xIndex < width) && (yIndex < height)) {
        uint  glob_ind  = yIndex * width + xIndex;
        uint  cache_ind = get_local_id(1)*(TILE+1)+get_local_id(0);
#if 0
        MyRec ne; loadNeutral(&ne);
        shmem_uchar[cache_ind] = (glob_ind < orig_size) ? idata1[glob_ind] : ne.c;
        shmem_ulong[cache_ind] = (glob_ind < orig_size) ? idata2[glob_ind] : ne.i;
        shmem_float[cache_ind] = (glob_ind < orig_size) ? idata3[glob_ind] : ne.f;
#else
        if(glob_ind < orig_size) {
            shmem_uchar[cache_ind] = idata1[glob_ind];
            shmem_ulong[cache_ind] = idata2[glob_ind];
            shmem_float[cache_ind] = idata3[glob_ind];
        } else {
            MyRec ne; loadNeutral(&ne);
            shmem_uchar[cache_ind] = ne.c;
            shmem_ulong[cache_ind] = ne.i;
            shmem_float[cache_ind] = ne.f;
        }
#endif
    }
 
    barrier(CLK_LOCAL_MEM_FENCE);

    // write the transposed matrix tile to global memory
    xIndex = get_group_id(1) * TILE + get_local_id(0);
    yIndex = get_group_id(0) * TILE + get_local_id(1);
    if((xIndex < height) && (yIndex < width)) {
        uint index_out = yIndex * height + xIndex;
        uint cache_ind = get_local_id(0)*(TILE+1)+get_local_id(1);
        
        odata1[index_out] = shmem_uchar[cache_ind];
        odata2[index_out] = shmem_ulong[cache_ind];
        odata3[index_out] = shmem_float[cache_ind];
    }
}

__kernel __attribute__((reqd_work_group_size(TILE, TILE, 1)))
void transposeFromPad(
        __global ftk_uchar* odata1, 
        __global ftk_ulong* odata2,
        __global ftk_float* odata3,  
        __global ftk_uchar* idata1,
        __global ftk_ulong* idata2,
        __global ftk_float* idata3, 
        uint                width, 
        uint                height,
        uint                orig_size
        // For some reason it does not seem to work
        // if one allocates ONE sh mem and uses pointer
        // arithmetic and casts to partition it into 3 parts!
//      , __local char*  shmem_char,
//        __local ulong* shmem_ulong,
//        __local float* shmem_float
) {
    uint xIndex, yIndex;
    __local ftk_uchar shmem_uchar[TILE*TILE+TILE];
    __local ftk_ulong shmem_ulong[TILE*TILE+TILE];
    __local ftk_float shmem_float[TILE*TILE+TILE];

    // read the matrix tile into shared memory
    xIndex = get_global_id(0);
    yIndex = get_global_id(1); 
  
    if((xIndex < width) && (yIndex < height))
    {
        uint  glob_ind = yIndex * width + xIndex;
        uint cache_ind = get_local_id(1)*(TILE+1)+get_local_id(0);

        shmem_uchar[cache_ind] = idata1[glob_ind];
        shmem_ulong[cache_ind] = idata2[glob_ind];
        shmem_float[cache_ind] = idata3[glob_ind];
    }
 
    barrier(CLK_LOCAL_MEM_FENCE);

    // write the transposed matrix tile to global memory
    xIndex = get_group_id(1) * TILE + get_local_id(0);
    yIndex = get_group_id(0) * TILE + get_local_id(1);
    if((xIndex < height) && (yIndex < width))
    {
        uint index_out = yIndex * height + xIndex;
        uint cache_ind = get_local_id(0)*(TILE+1)+get_local_id(1);

        if(index_out < orig_size) {
            odata1[index_out] = shmem_uchar[cache_ind];
            odata2[index_out] = shmem_ulong[cache_ind];
            odata3[index_out] = shmem_float[cache_ind];
        }
    }
}
#endif //ParBB_OpenCL


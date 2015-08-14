#ifndef ParBB_Wrappers
#define ParBB_Wrappers

typedef cl_float ftk_float;
typedef /*cl_ulong*/cl_uint ftk_ulong;
typedef cl_uchar ftk_uchar;

#include "DataStructs.h"

#define HWD_PAR_DEG 131072//65536//49152
//65536

#include <sys/time.h>
//#include <time.h> 

int timeval_subtract(struct timeval *result, struct timeval *t2, struct timeval *t1)
{
    unsigned int resolution=1000000;
    long int diff = (t2->tv_usec + resolution * t2->tv_sec) - (t1->tv_usec + resolution * t1->tv_sec);
    result->tv_sec = diff / resolution;
    result->tv_usec = diff % resolution;
    return (diff<0);
}


////////////////////////////////////////
/// Transpose Wrappers
////////////////////////////////////////

void transpadWrap(
    cl_command_queue    cqCommandQueue,
    cl_kernel           transpad_ker,
    unsigned int        width,
    unsigned int        height,
    unsigned int        arr_len, 
    cl_mem              arr_in1,
    cl_mem              arr_in2,
    cl_mem              arr_in3, 
    cl_mem              arr_out1,
    cl_mem              arr_out2,
    cl_mem              arr_out3
) {
    cl_int ciErr = 0, ciErr_tmp;
    unsigned int counter = 0;

    ciErr |= clSetKernelArg(transpad_ker, counter++, sizeof(cl_mem), (void*)&arr_out1);
    ciErr |= clSetKernelArg(transpad_ker, counter++, sizeof(cl_mem), (void*)&arr_out2);
    ciErr |= clSetKernelArg(transpad_ker, counter++, sizeof(cl_mem), (void*)&arr_out3);
    ciErr |= clSetKernelArg(transpad_ker, counter++, sizeof(cl_mem), (void*)&arr_in1);
    ciErr |= clSetKernelArg(transpad_ker, counter++, sizeof(cl_mem), (void*)&arr_in2);
    ciErr |= clSetKernelArg(transpad_ker, counter++, sizeof(cl_mem), (void*)&arr_in3);
    ciErr |= clSetKernelArg(transpad_ker, counter++, sizeof(unsigned int), (void*)&width);
    ciErr |= clSetKernelArg(transpad_ker, counter++, sizeof(unsigned int), (void*)&height);
    ciErr |= clSetKernelArg(transpad_ker, counter++, sizeof(unsigned int), (void*)&arr_len);
//    const unsigned int CACHE_SIZE = 8 * 256;
//    ciErr |= clSetKernelArg(transpad_ker, counter++, CACHE_SIZE*sizeof(ftk_uchar) , NULL);
//    ciErr |= clSetKernelArg(transpad_ker, counter++, CACHE_SIZE*sizeof(ftk_ulong), NULL);
//    ciErr |= clSetKernelArg(transpad_ker, counter++, CACHE_SIZE*sizeof(ftk_float), NULL);
    oclCheckError(ciErr, CL_SUCCESS);

    {
        size_t  width1  = ((width +TILE-1) / TILE) * TILE;
        size_t  height1 = ((height+TILE-1) / TILE) * TILE;
        size_t  globalWorkSize[2] = { width1, height1 };
        size_t   localWorkSize[2] = { TILE, TILE };

        ciErr |= clEnqueueNDRangeKernel(
                        cqCommandQueue, transpad_ker, 2, NULL,
                        globalWorkSize, localWorkSize, 0, NULL, NULL
                  );

        ciErr |= clFinish(cqCommandQueue);
        oclCheckError(ciErr, CL_SUCCESS);
    }
}

////////////////////////////////////////
/// Scan Wrappers
////////////////////////////////////////

void scanInc(
    cl_command_queue    cqCommandQueue,
    cl_context          cxGPUContext,
    cl_kernel           inc_scan_inchunk_ker,
    cl_kernel           inc_scan_block_ker,
    cl_kernel           inc_scan_outchunk_ker,
    cl_kernel           transp_topad_ker,
    cl_kernel           transp_frompad_ker,
    unsigned int        block_size,
    unsigned int        arr_len, 
    cl_mem              arr_in1,
    cl_mem              arr_in2,
    cl_mem              arr_in3,  
    cl_mem              arr_out1,
    cl_mem              arr_out2,
    cl_mem              arr_out3
) {
    struct timeval t_beg, t_end, t_diff;
    unsigned long int elapsed = 0, elapsed_all = 0;

    cl_int ciErr = 0, ciErr_tmp;

    // make parallel work be a multiple of block_size
    unsigned int par_work  = (HWD_PAR_DEG <= arr_len) ? 
                                ((HWD_PAR_DEG+block_size -1) / block_size) * block_size :
                                ((arr_len    +block_size -1) / block_size) * block_size ;
    unsigned int num_blocks= par_work / block_size;

    // the chunk that is scanned sequentially
    unsigned int seq_chunk = (arr_len + par_work - 1) / par_work;

    // size of the padded array obtained after (padded) transposition
    unsigned int arr_len_pad = par_work * seq_chunk;

    if(num_blocks > block_size) {
        printf( "ERROR: number of blocks %d greater than block size %d!\n\n", 
                num_blocks, block_size);
        exit(1);
    }

    printf( "IncScan: block_size: %d, par_work: %d, seq_chunk: %d, arr_len_pad: %d, size(long): %lu\n\n"
            , block_size, par_work, seq_chunk, arr_len_pad, sizeof(ftk_ulong));    

    cl_mem arr_tr1; cl_mem arr_tr2; cl_mem arr_tr3;
    cl_mem tmp1; cl_mem tmp2; cl_mem tmp3;
    cl_mem blk1; cl_mem blk2; cl_mem blk3;
    {
        // transposition arrays!
        arr_tr1 = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE, arr_len_pad*sizeof(ftk_uchar), NULL, &ciErr_tmp );
        ciErr |= ciErr_tmp;
        arr_tr2 = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE, arr_len_pad*sizeof(ftk_ulong), NULL, &ciErr_tmp );
        ciErr |= ciErr_tmp;
        arr_tr3 = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE, arr_len_pad*sizeof(ftk_float), NULL, &ciErr_tmp );
        ciErr |= ciErr_tmp;
        oclCheckError(ciErr, CL_SUCCESS);

        // one elems/block reduce-compressed arrays!
        tmp1 = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE, num_blocks*sizeof(ftk_uchar), NULL, &ciErr_tmp );
        ciErr |= ciErr_tmp;
        tmp2 = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE, num_blocks*sizeof(ftk_ulong), NULL, &ciErr_tmp );
        ciErr |= ciErr_tmp;
        tmp3 = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE, num_blocks*sizeof(ftk_float), NULL, &ciErr_tmp );
        ciErr |= ciErr_tmp;
        oclCheckError(ciErr, CL_SUCCESS);

        // block elems/block scan-compressed arrays!
        blk1 = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE, par_work*sizeof(ftk_uchar), NULL, &ciErr_tmp );
        ciErr |= ciErr_tmp;
        blk2 = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE, par_work*sizeof(ftk_ulong), NULL, &ciErr_tmp );
        ciErr |= ciErr_tmp;
        blk3 = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE, par_work*sizeof(ftk_float), NULL, &ciErr_tmp );
        ciErr |= ciErr_tmp;
        ciErr |= clFinish(cqCommandQueue);
        oclCheckError(ciErr, CL_SUCCESS);
    }

    // Just warm up the GPU
    for(int warmup=0; warmup<4; warmup++) {
        transpadWrap( cqCommandQueue, transp_topad_ker, 
                      seq_chunk, par_work, arr_len,
                      arr_in1, arr_in2, arr_in3, 
                      arr_tr1, arr_tr2, arr_tr3  );        
    }

    gettimeofday(&t_beg, NULL);

    // transpose the arrays!
    transpadWrap( cqCommandQueue, transp_topad_ker, 
                  seq_chunk, par_work, arr_len,
                  arr_in1, arr_in2, arr_in3, 
                  arr_tr1, arr_tr2, arr_tr3  );

    gettimeofday(&t_end, NULL);
    timeval_subtract(&t_diff, &t_end, &t_beg);
    elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
    elapsed_all += elapsed;
    printf("First Transpose Time: %lu microsecs.\n", elapsed);


    gettimeofday(&t_beg, NULL);

    // Call thread-level scan that also does a block scan at the end:
    // thread-level summary in tmp1..3, block-level summary in blk1..3!
    {
        cl_kernel ker = inc_scan_inchunk_ker;
        unsigned int counter = 0;
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&tmp1);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&tmp2);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&tmp3);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&blk1);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&blk2);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&blk3);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&arr_tr1);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&arr_tr2);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&arr_tr3);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(unsigned int), (void*)&seq_chunk);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(unsigned int), (void*)&arr_len_pad);
        // shared memory declarations:
        ciErr |= clSetKernelArg(ker, counter++, block_size*sizeof(ftk_uchar) , NULL);
        ciErr |= clSetKernelArg(ker, counter++, block_size*sizeof(ftk_ulong), NULL);
        ciErr |= clSetKernelArg(ker, counter++, block_size*sizeof(ftk_float), NULL);
        oclCheckError(ciErr, CL_SUCCESS);
        
        {
            size_t  globalWorkSize = par_work;
            size_t  localWorkSize  = block_size;
            ciErr |= clEnqueueNDRangeKernel(
                        cqCommandQueue, ker, 1, NULL,
                        &globalWorkSize, &localWorkSize, 0, NULL, NULL
                    );
            //shrLog(stdlog, "ERROR: %d, %d!\n", ciErr, CL_INVALID_WORK_GROUP_SIZE);
        }
        ciErr |= clFinish(cqCommandQueue);
        oclCheckError(ciErr, CL_SUCCESS);
    }

    gettimeofday(&t_end, NULL);
    timeval_subtract(&t_diff, &t_end, &t_beg);
    elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
    elapsed_all += elapsed;
    printf("Scan Shrink Time: %lu microsecs.\n", elapsed);

    gettimeofday(&t_beg, NULL);

    // Scan the remaining block in one go!
    if(num_blocks > 1) {
        cl_kernel ker = inc_scan_block_ker;
        unsigned int counter = 0;
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&tmp1);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&tmp2);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&tmp3);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&tmp1);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&tmp2);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&tmp3);

        ciErr |= clSetKernelArg(ker, counter++, sizeof(unsigned int), (void*)&num_blocks);

        ciErr |= clSetKernelArg(ker, counter++, block_size*sizeof(ftk_uchar) , NULL);
        ciErr |= clSetKernelArg(ker, counter++, block_size*sizeof(ftk_ulong), NULL);
        ciErr |= clSetKernelArg(ker, counter++, block_size*sizeof(ftk_float), NULL);
        oclCheckError(ciErr, CL_SUCCESS);
        
        {
            size_t  globalWorkSize = block_size;//num_blocks;
            size_t  localWorkSize  = block_size;
            ciErr |= clEnqueueNDRangeKernel(
                        cqCommandQueue, ker, 1, NULL,
                        &globalWorkSize, &localWorkSize, 0, NULL, NULL
                    );
        }

        oclCheckError(ciErr, CL_SUCCESS);
        ciErr |= clFinish(cqCommandQueue);        
    }

    gettimeofday(&t_end, NULL);
    timeval_subtract(&t_diff, &t_end, &t_beg);
    elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
    elapsed_all += elapsed;
    printf("Scan Block Time: %lu microsecs.\n", elapsed);

    gettimeofday(&t_beg, NULL);

    // redo the computation based on the scanned block-level (partial) results
    {
        cl_kernel ker = inc_scan_outchunk_ker;
        unsigned int counter = 0;
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&arr_tr1);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&arr_tr2);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&arr_tr3);

        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&tmp1);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&tmp2);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&tmp3);

        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&blk1);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&blk2);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(cl_mem), (void*)&blk3);

        ciErr |= clSetKernelArg(ker, counter++, sizeof(unsigned int), (void*)&seq_chunk);
        ciErr |= clSetKernelArg(ker, counter++, sizeof(unsigned int), (void*)&arr_len_pad);

//        ciErr |= clSetKernelArg(ker, counter++, block_size*sizeof(ftk_uchar) , NULL);
//        ciErr |= clSetKernelArg(ker, counter++, block_size*sizeof(ftk_ulong), NULL);
//        ciErr |= clSetKernelArg(ker, counter++, block_size*sizeof(ftk_float), NULL);
        oclCheckError(ciErr, CL_SUCCESS);
        
        {
            size_t  globalWorkSize = par_work;
            size_t  localWorkSize  = block_size;
            ciErr |= clEnqueueNDRangeKernel(
                        cqCommandQueue, ker, 1, NULL,
                        &globalWorkSize, &localWorkSize, 0, NULL, NULL
                    );
            //shrLog(stdlog, "ERROR: %d, %d!\n", ciErr, CL_INVALID_WORK_GROUP_SIZE);
        }

        oclCheckError(ciErr, CL_SUCCESS);
        ciErr |= clFinish(cqCommandQueue);
    }

    gettimeofday(&t_end, NULL);
    timeval_subtract(&t_diff, &t_end, &t_beg);
    elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
    elapsed_all += elapsed;
    printf("Scan Expand Time: %lu microsecs.\n", elapsed);

    gettimeofday(&t_beg, NULL);

    // finally, transpose back if needed!
    transpadWrap (   
        cqCommandQueue, transp_frompad_ker,
        par_work, seq_chunk, arr_len,
        arr_tr1, arr_tr2, arr_tr3,
        arr_out1, arr_out2, arr_out3  );

    gettimeofday(&t_end, NULL);
    timeval_subtract(&t_diff, &t_end, &t_beg);
    elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
    elapsed_all += elapsed;
    printf("Second Transpose Time: %lu microsecs.\n", elapsed);
    printf("\nTOTAL SCAN INCLUSIVE TIME: %lu microsecs\n\n\n", elapsed_all);

    { // free intermediate memory
        clReleaseMemObject(tmp1);
        clReleaseMemObject(tmp2);
        clReleaseMemObject(tmp3);
        clReleaseMemObject(blk1);
        clReleaseMemObject(blk2);
        clReleaseMemObject(blk3);
        clReleaseMemObject(arr_tr1);
        clReleaseMemObject(arr_tr2);
        clReleaseMemObject(arr_tr3);
    }
}

#endif //ParBB_Wrappers

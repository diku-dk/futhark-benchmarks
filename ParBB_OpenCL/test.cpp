#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <assert.h>
#include "SDK_stub.h"
#include "ParBBWrappers.h"
//#include <cstdlib>


const unsigned int N = 32*1024*1024 - 23;
ftk_uchar arr_char[N];
ftk_ulong arr_lint[N];
ftk_float arr_flot[N];
ftk_uchar out_char[N];
ftk_ulong out_lint[N];
ftk_float out_flot[N];

void initArrays() {
    unsigned int i;
    srand(333/*time(NULL)*/);
    for(i=0; i<N; i++) {
        unsigned int ri = rand();
        arr_lint[i]  = ri;
        arr_flot[i]  = ((ftk_float)ri) / RAND_MAX;
        arr_char[i]  = ((ri % 5) == 0) ? 1 : 0; 
    }
}

void testPaddedTransposition (
        cl_command_queue&   cqCommandQueue,
        cl_program          cpProgram,
        cl_context&         cxGPUContext
) {
    struct timeval t_beg, t_end, t_diff;
    unsigned long int elapsed = 0, elapsed_all = 0;

    unsigned int width  = (N + HWD_PAR_DEG - 1) / HWD_PAR_DEG;
    unsigned int height = HWD_PAR_DEG;

    cl_int ciErr = 0, ciErr_tmp;
    cl_mem  gpu_inp_char, gpu_inp_lint, gpu_inp_flot,
            gpu_out_char, gpu_out_lint, gpu_out_flot;
    { // copy arrays from host
        gpu_inp_char = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, 
            N*sizeof(ftk_uchar), arr_char, &ciErr_tmp );
        ciErr |= ciErr_tmp;

        gpu_inp_lint = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, 
            N*sizeof(ftk_ulong), arr_lint, &ciErr_tmp );
        ciErr |= ciErr_tmp;

        gpu_inp_flot = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, 
            N*sizeof(ftk_float), arr_flot, &ciErr_tmp );
        ciErr |= ciErr_tmp;

        gpu_out_char = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE, 
            width*height*sizeof(ftk_uchar), NULL, &ciErr_tmp );
        ciErr |= ciErr_tmp;

        gpu_out_lint = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE, 
            width*height*sizeof(ftk_ulong), NULL, &ciErr_tmp );
        ciErr |= ciErr_tmp;
        oclCheckError(ciErr, CL_SUCCESS);
        gpu_out_flot = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE, 
            width*height*sizeof(ftk_float), NULL, &ciErr_tmp );
        ciErr |= ciErr_tmp;

        oclCheckError(ciErr, CL_SUCCESS);
    }

    // make kernels
    cl_kernel transp_topad_ker, transp_frompad_ker;
    {
        transp_topad_ker   = clCreateKernel(cpProgram, "transposeToPad",   &ciErr);
        oclCheckError(ciErr, CL_SUCCESS);
        transp_frompad_ker = clCreateKernel(cpProgram, "transposeFromPad", &ciErr);
        oclCheckError(ciErr, CL_SUCCESS);
    }

    { // transpose forth and back
        // warmup the device first!
        for(int warmup=0; warmup<4; warmup++) {
            transpadWrap( cqCommandQueue, transp_topad_ker, 
                          width,   height,  N, 
                          gpu_inp_char, gpu_inp_lint, gpu_inp_flot,
                          gpu_out_char, gpu_out_lint, gpu_out_flot );
            transpadWrap( cqCommandQueue, transp_frompad_ker, 
                          height,  width,   N, 
                          gpu_out_char,   gpu_out_lint, gpu_out_flot,
                          gpu_inp_char,   gpu_inp_lint, gpu_inp_flot );
        }

        gettimeofday(&t_beg, NULL);

        transpadWrap( cqCommandQueue, transp_topad_ker, 
                      width,   height,  N, 
                      gpu_inp_char, gpu_inp_lint, gpu_inp_flot,
                      gpu_out_char, gpu_out_lint, gpu_out_flot );

        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_beg);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
        elapsed_all += elapsed;
        printf("Transpose FORTH Time: %lu microsecs.\n", elapsed);

        gettimeofday(&t_beg, NULL);

        transpadWrap( cqCommandQueue, transp_frompad_ker, 
                      height,  width,   N, 
                      gpu_out_char,   gpu_out_lint, gpu_out_flot,
                      gpu_inp_char,   gpu_inp_lint, gpu_inp_flot );

        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_beg);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
        elapsed_all += elapsed;
        printf("Transpose BACK Time: %lu microsecs.\n", elapsed);
    }

    { // copy back!
        ciErr = clEnqueueReadBuffer(
                            cqCommandQueue, gpu_inp_char, CL_TRUE, 0,
                            N*sizeof(ftk_uchar), out_char, 0, NULL, NULL
                    );
        ciErr |= clFinish(cqCommandQueue);
        oclCheckError(ciErr, CL_SUCCESS);

        ciErr = clEnqueueReadBuffer(
                            cqCommandQueue, gpu_inp_lint, CL_TRUE, 0,
                            N*sizeof(ftk_ulong), out_lint, 0, NULL, NULL
                    );
        ciErr |= clFinish(cqCommandQueue);
        oclCheckError(ciErr, CL_SUCCESS);

        ciErr = clEnqueueReadBuffer(
                            cqCommandQueue, gpu_inp_flot, CL_TRUE, 0,
                            N*sizeof(ftk_float), out_flot, 0, NULL, NULL
                    );
        ciErr |= clFinish(cqCommandQueue);
        oclCheckError(ciErr, CL_SUCCESS);
    }

    clReleaseKernel(transp_topad_ker);
    clReleaseKernel(transp_frompad_ker);

    clReleaseMemObject(gpu_inp_char);
    clReleaseMemObject(gpu_out_char);
    clReleaseMemObject(gpu_inp_lint);
    clReleaseMemObject(gpu_out_lint);
    clReleaseMemObject(gpu_inp_flot);
    clReleaseMemObject(gpu_out_flot);

    // validation
    unsigned int ok = N;
    for(int i=0; i<N; i++) {
        if (arr_lint[i] != out_lint[i] ||
            arr_char[i] != out_char[i] ||
            arr_flot[i] != out_flot[i]  ) {
            ok = i; 
            break;
        }
    }

    if(ok == N) {
        printf("VALID RESULT FOR TRANSPOSE FORTH-BACK!!!\n\n");
    } else {
        printf("INVALID RESULT FOR TRANSPOSE FORTH-BACK at index: %d should be %lu is %lu!!!\n\n"
                , ok, arr_lint[ok], out_lint[ok]);
    }
}

void testScanInc (
        cl_command_queue&   cqCommandQueue,
        cl_program          cpProgram,
        cl_context&         cxGPUContext
) {
    cl_int ciErr = 0, ciErr_tmp;
    cl_mem  gpu_inp_char, gpu_inp_lint, gpu_inp_flot,
            gpu_out_char, gpu_out_lint, gpu_out_flot;
    { // copy arrays from host
        gpu_inp_char = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, 
            N*sizeof(ftk_uchar), arr_char, &ciErr_tmp );
        ciErr |= ciErr_tmp;

        gpu_inp_lint = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, 
            N*sizeof(ftk_ulong), arr_lint, &ciErr_tmp );
        ciErr |= ciErr_tmp;

        gpu_inp_flot = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, 
            N*sizeof(ftk_float), arr_flot, &ciErr_tmp );
        ciErr |= ciErr_tmp;

        gpu_out_char = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE, 
            N*sizeof(ftk_uchar), NULL, &ciErr_tmp );
        ciErr |= ciErr_tmp;

        gpu_out_lint = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE, 
            N*sizeof(ftk_ulong), NULL, &ciErr_tmp );
        ciErr |= ciErr_tmp;
        oclCheckError(ciErr, CL_SUCCESS);
        gpu_out_flot = clCreateBuffer (
            cxGPUContext, CL_MEM_READ_WRITE, 
            N*sizeof(ftk_float), NULL, &ciErr_tmp );
        ciErr |= ciErr_tmp;

        oclCheckError(ciErr, CL_SUCCESS);
    }

    // make kernels
    cl_kernel transp_topad_ker, transp_frompad_ker;
    cl_kernel scan_shrink_ker, scan_1block_ker, scan_expand_ker;
    {
        transp_topad_ker   = clCreateKernel(cpProgram, "transposeToPad",   &ciErr);
        oclCheckError(ciErr, CL_SUCCESS);
        transp_frompad_ker = clCreateKernel(cpProgram, "transposeFromPad", &ciErr);
        oclCheckError(ciErr, CL_SUCCESS);
        scan_shrink_ker = clCreateKernel(cpProgram, "scanIncShrinkMyRecKer", &ciErr);
        oclCheckError(ciErr, CL_SUCCESS);
        scan_1block_ker = clCreateKernel(cpProgram, "scanIncBlockMyRecKer",  &ciErr);
        oclCheckError(ciErr, CL_SUCCESS);
        scan_expand_ker = clCreateKernel(cpProgram, "scanIncExpandMyRecKer", &ciErr);
        oclCheckError(ciErr, CL_SUCCESS);
    }
    
    unsigned int block_size = 512;//256;
    { // call scan wrapper:
        scanInc( cqCommandQueue, cxGPUContext 
               , scan_shrink_ker, scan_1block_ker, scan_expand_ker 
               , transp_topad_ker, transp_frompad_ker, block_size, N
               , gpu_inp_char, gpu_inp_lint, gpu_inp_flot
               , gpu_out_char, gpu_out_lint, gpu_out_flot
               );
    }

    { // copy back!
        ciErr = clEnqueueReadBuffer(
                            cqCommandQueue, gpu_out_char, CL_TRUE, 0,
                            N*sizeof(ftk_uchar), out_char, 0, NULL, NULL
                    );
        ciErr |= clFinish(cqCommandQueue);
        oclCheckError(ciErr, CL_SUCCESS);

        ciErr = clEnqueueReadBuffer(
                            cqCommandQueue, gpu_out_lint, CL_TRUE, 0,
                            N*sizeof(ftk_ulong), out_lint, 0, NULL, NULL
                    );
        ciErr |= clFinish(cqCommandQueue);
        oclCheckError(ciErr, CL_SUCCESS);

        ciErr = clEnqueueReadBuffer(
                            cqCommandQueue, gpu_out_flot, CL_TRUE, 0,
                            N*sizeof(ftk_float), out_flot, 0, NULL, NULL
                    );
        ciErr |= clFinish(cqCommandQueue);
        oclCheckError(ciErr, CL_SUCCESS);
    }

    // release kernels:
    clReleaseKernel(transp_topad_ker);
    clReleaseKernel(transp_frompad_ker);
    clReleaseKernel(scan_shrink_ker);
    clReleaseKernel(scan_1block_ker);
    clReleaseKernel(scan_expand_ker);

    // release memory:    
    clReleaseMemObject(gpu_inp_char);
    clReleaseMemObject(gpu_out_char);
    clReleaseMemObject(gpu_inp_lint);
    clReleaseMemObject(gpu_out_lint);
    clReleaseMemObject(gpu_inp_flot);
    clReleaseMemObject(gpu_out_flot);

    { // Seqential execution:
        struct timeval t_beg, t_end, t_diff;
        unsigned long int elapsed = 0;
        gettimeofday(&t_beg, NULL);

        MyRec dst, src1, src2;
        loadNeutral(&dst);
        for(unsigned int i=0; i<N; i++) {
            src2.c = arr_char[i]; src2.i = arr_lint[i]; src2.f = arr_flot[i];
            src1.c = dst.c;       src1.i = dst.i;       src1.f = dst.f;
            applyMyRecOP(&dst, &src1, &src2);
            arr_char[i] = dst.c;  arr_lint[i] = dst.i;  arr_flot[i] = dst.f;
        }

        gettimeofday(&t_end, NULL);
        timeval_subtract(&t_diff, &t_end, &t_beg);
        elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec); 
        printf("\nGolden Sequential Scan Time: %lu microsecs.\n\n\n", elapsed);
    }

    // Validation:
    unsigned int i, ok = N;
    for(i=0; i<N; i++) {
        if( arr_char[i] != out_char[i] || 
            arr_lint[i] != out_lint[i] || 
            fabs(arr_flot[i] - out_flot[i])/arr_flot[i] >= 0.0005 
        ) {
            if(ok == N) {
                ok = i;
                printf( "ERROR at ind %d, is: (%d,%lu,%f), ", i
                      , (unsigned int)(out_char[ok]), out_lint[ok], out_flot[ok] );
                printf( "should be: (%d,%lu,%f), cur_el: (%d,%lu,%f)\n\n"
                      , (unsigned int)out_char[i], out_lint[i], out_flot[i]
                      , (unsigned int)arr_char[i], arr_lint[i], arr_flot[i] );
                break;
            }
        }
    }

    if(ok == N) {
        printf("\n\nVALID   YEY RESULT FOR INCLUSIVE SCAN!!!\n\n");
    } else {
        printf("\n\nINVALID NAY RESULT FOR INCLUSIVE SCAN!!!\n\n");
    }
}


int main() {
    cl_context cxGPUContext;                        // OpenCL context
    cl_command_queue cqCommandQueue[16];            // OpenCL command que
    cl_uint nDevice;                                // OpenCL device count
    cl_device_id* cdDevices;                        // OpenCL device list
    cl_program cpProgram;                           // OpenCL program
    unsigned int dev_id = 0;

    { // making command queue, building program, etc
        char  compile_opts[128];
        sprintf( compile_opts, "-D lgWARP=%d", lgWARP );

        build_for_GPU(
            cxGPUContext, cqCommandQueue,
            nDevice, cdDevices, cpProgram, dev_id, compile_opts, "", "ParBBKernels"
        );
    }
    
    initArrays();
    testPaddedTransposition(cqCommandQueue[dev_id], cpProgram, cxGPUContext);
    testScanInc            (cqCommandQueue[dev_id], cpProgram, cxGPUContext);

    // release resources
    clReleaseProgram(cpProgram);
    clReleaseCommandQueue(cqCommandQueue[dev_id]);
    clReleaseContext(cxGPUContext);
}

#ifndef SDK_INTERNALS
#define SDK_INTERNALS

#include <string.h>
#include <iostream>

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/opencl.h>
#endif

/**********************************/
/******* MACROS            ********/
/**********************************/

#if 0
#define CREATE_AND_ENQUEUE_RO_CL_BUFFER(X) \
        ocl_arrs.X = clCreateBuffer( cxGPUContext, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, \
                                                            cur_size, X, &ciErr2 ); \
        ciErr |= ciErr2;

#define CREATE_AND_ENQUEUE_GPU_ONLY_CL_BUFFER(X) \
        ocl_arrs.X = clCreateBuffer( cxGPUContext, CL_MEM_READ_WRITE, \
                                                            cur_size, NULL, &ciErr2 ); \
        ciErr |= ciErr2;

#define CREATE_AND_ENQUEUE_OUT_CL_BUFFER(X) \
        ocl_arrs.X = clCreateBuffer( cxGPUContext, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, \
                                                            cur_size, X, &ciErr2 ); \
        ciErr |= ciErr2;

#define CREATE_AND_ENQUEUE_WO_CL_BUFFER(X) \
        ocl_arrs.X = clCreateBuffer( cxGPUContext, CL_MEM_WRITE_ONLY, \
                                                            cur_size, NULL, &ciErr2 ); \
        ciErr |= ciErr2;

#else

#define CREATE_AND_ENQUEUE_RO_CL_BUFFER(X) \
        ocl_arrs.X = clCreateBuffer( cxGPUContext, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, \
                                                            cur_size, cpu_arrs.X, &ciErr2 ); \
        ciErr |= ciErr2;

#define CREATE_AND_ENQUEUE_GPU_ONLY_CL_BUFFER(X) \
        ocl_arrs.X = clCreateBuffer( cxGPUContext, CL_MEM_READ_WRITE, \
                                                            cur_size, NULL, &ciErr2 ); \
        ciErr |= ciErr2;

#define CREATE_AND_ENQUEUE_OUT_CL_BUFFER(X) \
        ocl_arrs.X = clCreateBuffer( cxGPUContext, CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR, \
                                                            cur_size, cpu_arrs.X, &ciErr2 ); \
        ciErr |= ciErr2;

#endif

/**********************************/
/******* FROM oclUtils.h   ********/
/**********************************/

const char* oclErrorString(unsigned int err) 
{
    switch (err) {
        case CL_SUCCESS:                            return "Success!";
        case CL_DEVICE_NOT_FOUND:                   return "Device not found.";
        case CL_DEVICE_NOT_AVAILABLE:               return "Device not available";
        case CL_COMPILER_NOT_AVAILABLE:             return "Compiler not available";
        case CL_MEM_OBJECT_ALLOCATION_FAILURE:      return "Memory object allocation failure";
        case CL_OUT_OF_RESOURCES:                   return "Out of resources";
        case CL_OUT_OF_HOST_MEMORY:                 return "Out of host memory";
        case CL_PROFILING_INFO_NOT_AVAILABLE:       return "Profiling information not available";
        case CL_MEM_COPY_OVERLAP:                   return "Memory copy overlap";
        case CL_IMAGE_FORMAT_MISMATCH:              return "Image format mismatch";
        case CL_IMAGE_FORMAT_NOT_SUPPORTED:         return "Image format not supported";
        case CL_BUILD_PROGRAM_FAILURE:              return "Program build failure";
        case CL_MAP_FAILURE:                        return "Map failure";
        case CL_INVALID_VALUE:                      return "Invalid value";
        case CL_INVALID_DEVICE_TYPE:                return "Invalid device type";
        case CL_INVALID_PLATFORM:                   return "Invalid platform";
        case CL_INVALID_DEVICE:                     return "Invalid device";
        case CL_INVALID_CONTEXT:                    return "Invalid context";
        case CL_INVALID_QUEUE_PROPERTIES:           return "Invalid queue properties";
        case CL_INVALID_COMMAND_QUEUE:              return "Invalid command queue";
        case CL_INVALID_HOST_PTR:                   return "Invalid host pointer";
        case CL_INVALID_MEM_OBJECT:                 return "Invalid memory object";
        case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:    return "Invalid image format descriptor";
        case CL_INVALID_IMAGE_SIZE:                 return "Invalid image size";
        case CL_INVALID_SAMPLER:                    return "Invalid sampler";
        case CL_INVALID_BINARY:                     return "Invalid binary";
        case CL_INVALID_BUILD_OPTIONS:              return "Invalid build options";
        case CL_INVALID_PROGRAM:                    return "Invalid program";
        case CL_INVALID_PROGRAM_EXECUTABLE:         return "Invalid program executable";
        case CL_INVALID_KERNEL_NAME:                return "Invalid kernel name";
        case CL_INVALID_KERNEL_DEFINITION:          return "Invalid kernel definition";
        case CL_INVALID_KERNEL:                     return "Invalid kernel";
        case CL_INVALID_ARG_INDEX:                  return "Invalid argument index";
        case CL_INVALID_ARG_VALUE:                  return "Invalid argument value";
        case CL_INVALID_ARG_SIZE:                   return "Invalid argument size";
        case CL_INVALID_KERNEL_ARGS:                return "Invalid kernel arguments";
        case CL_INVALID_WORK_DIMENSION:             return "Invalid work dimension";
        case CL_INVALID_WORK_GROUP_SIZE:            return "Invalid work group size";
        case CL_INVALID_WORK_ITEM_SIZE:             return "Invalid work item size";
        case CL_INVALID_GLOBAL_OFFSET:              return "Invalid global offset";
        case CL_INVALID_EVENT_WAIT_LIST:            return "Invalid event wait list";
        case CL_INVALID_EVENT:                      return "Invalid event";
        case CL_INVALID_OPERATION:                  return "Invalid operation";
        case CL_INVALID_GL_OBJECT:                  return "Invalid OpenGL object";
        case CL_INVALID_BUFFER_SIZE:                return "Invalid buffer size";
        case CL_INVALID_MIP_LEVEL:                  return "Invalid mip-map level";
        default:                                    return "Unknown";
    }
}

    #define shrLog      fprintf
    #define shrLogEx    fprintf
    #define stdlog      stderr

    enum shrBOOL
    {
        shrFALSE = 0,
        shrTRUE = 1
    };

    #define MIN(a,b) ((a<=b) ? a : b)
    #define MAX(a,b) ((a>=b) ? a : b)

    #define HDASHLINE "-----------------------------------------------------------\n"

    #define oclCheckErrorEX(a, b, c) __oclCheckErrorEX(a, b, c, __FILE__ , __LINE__)
    #define oclCheckError(a, b) oclCheckErrorEX(a, b, 0)

    void __oclCheckErrorEX(cl_int iSample, cl_int iReference, void (*pCleanup)(int), const char* cFile, const int iLine)
    {
        // An error condition is defined by the sample/test value not equal to the reference
        if (iReference != iSample)
        {
            // If the sample/test value isn't equal to the ref, it's an error by defnition, so override 0 sample/test value
            iSample = (iSample == 0) ? -9999 : iSample;

            // Log the error info
            shrLog(stdlog, "\n !!! Error # %i (%s) at line %i , in file %s !!!\n\n", iSample, oclErrorString(iSample), iLine, cFile);

            // Cleanup and exit, or just exit if no cleanup function pointer provided.  Use iSample (error code in this case) as process exit code.
            if (pCleanup != NULL)
            {
                pCleanup(iSample);
            }
            else
            {
                shrLogEx(stdlog, "Exiting...\n");
                exit(iSample);
            }
        }
    }

    inline int ConvertSMVer2CoresCopy(int major, int minor)
    {
        // Defines for GPU Architecture types (using the SM version to determine the # of cores per SM
        typedef struct {
            int SM; // 0xMm (hexidecimal notation), M = SM Major version, and m = SM minor version
            int Cores;
        } sSMtoCores;

        sSMtoCores nGpuArchCoresPerSM[] =
        { { 0x10,  8 }, // Tesla Generation (SM 1.0) G80 class
          { 0x11,  8 }, // Tesla Generation (SM 1.1) G8x class
          { 0x12,  8 }, // Tesla Generation (SM 1.2) G9x class
          { 0x13,  8 }, // Tesla Generation (SM 1.3) GT200 class
          { 0x20, 32 }, // Fermi Generation (SM 2.0) GF100 class
          { 0x21, 48 }, // Fermi Generation (SM 2.1) GF10x class
          {   -1, -1 }
        };

        int index = 0;
        while (nGpuArchCoresPerSM[index].SM != -1) {
            if (nGpuArchCoresPerSM[index].SM == ((major << 4) + minor) ) {
                return nGpuArchCoresPerSM[index].Cores;
            }
            index++;
        }
        fprintf(stderr, "MapSMtoCores SM %d.%d is undefined (please update to the latest SDK)!\n", major, minor);
        return 8;
    }

    //////////////////////////////////////////////////////////////////////////////
    //! Gets the platform ID for NVIDIA if available, otherwise default
    //!
    //! @return the id
    //! @param clSelectedPlatformID         OpenCL platoform ID
    //////////////////////////////////////////////////////////////////////////////
    cl_int oclGetPlatformID(cl_platform_id* clSelectedPlatformID)
    {
        char chBuffer[1024];
        cl_uint num_platforms;
        cl_platform_id* clPlatformIDs;
        cl_int ciErrNum;
        *clSelectedPlatformID = NULL;

        // Get OpenCL platform count
        ciErrNum = clGetPlatformIDs (0, NULL, &num_platforms);
        if (ciErrNum != CL_SUCCESS)
        {
            shrLog(stdlog, " Error %i in clGetPlatformIDs Call !!!\n\n", ciErrNum);
            return -1000;
        }
        else
        {
            if(num_platforms == 0)
            {
                shrLog(stdlog, "No OpenCL platform found!\n\n");
                return -2000;
            }
            else
            {
                // if there's a platform or more, make space for ID's
                if ((clPlatformIDs = (cl_platform_id*)malloc(num_platforms * sizeof(cl_platform_id))) == NULL)
                {
                    shrLog(stdlog, "Failed to allocate memory for cl_platform ID's!\n\n");
                    return -3000;
                }

    	    	shrLog(stdlog, "Found %d platform(s)\n", num_platforms);

                // get platform info for each platform and trap the NVIDIA platform if found
                ciErrNum = clGetPlatformIDs (num_platforms, clPlatformIDs, NULL);
                for(cl_uint i = 0; i < num_platforms; ++i)
                {
                    ciErrNum = clGetPlatformInfo (clPlatformIDs[i], CL_PLATFORM_VENDOR, 1024, &chBuffer, NULL);
                    if(ciErrNum == CL_SUCCESS)
                    {
  		                shrLog(stdlog, "Platform name is %s\n", chBuffer);
                        if( strstr(chBuffer, "NVIDIA") != NULL || 
                            strstr(chBuffer, "Apple")  != NULL || 
                            strstr(chBuffer, "Advanced") != NULL )
                        {
                            *clSelectedPlatformID = clPlatformIDs[i];
                            break;
                        }
                    }
                }

                // default to zeroeth platform if NVIDIA not found
                if(*clSelectedPlatformID == NULL)
                {
                    shrLog(stdlog, "WARNING: NVIDIA OpenCL platform not found - defaulting to first platform!\n\n");
                    *clSelectedPlatformID = clPlatformIDs[0];
                }

                free(clPlatformIDs);
            }
        }

        return CL_SUCCESS;
    }

    char* oclLoadProgSource(const char* cFilename, const char* cPreamble, size_t* szFinalLength)
    {
        // locals
        FILE* pFileStream = NULL;
        size_t szSourceLength;

        // open the OpenCL source code file
        #ifdef _WIN32   // Windows version
            if(fopen_s(&pFileStream, cFilename, "rb") != 0)
            {
                return NULL;
            }
        #else           // Linux version
            pFileStream = fopen(cFilename, "rb");
            if(pFileStream == 0)
            {
                return NULL;
            }
        #endif

        size_t szPreambleLength = strlen(cPreamble);

        // get the length of the source code
        fseek(pFileStream, 0, SEEK_END);
        szSourceLength = ftell(pFileStream);
        fseek(pFileStream, 0, SEEK_SET);

        // allocate a buffer for the source code string and read it in
        char* cSourceString = (char *)malloc(szSourceLength + szPreambleLength + 1);
        memcpy(cSourceString, cPreamble, szPreambleLength);
        if (fread((cSourceString) + szPreambleLength, szSourceLength, 1, pFileStream) != 1)
        {
            fclose(pFileStream);
            free(cSourceString);
            return 0;
        }

        // close the file and return the total length of the combined (preamble + source) string
        fclose(pFileStream);
        if(szFinalLength != 0)
        {
            *szFinalLength = szSourceLength + szPreambleLength;
        }
        cSourceString[szSourceLength + szPreambleLength] = '\0';

        return cSourceString;
    }

    void oclLogPtx(cl_program cpProgram, cl_device_id cdDevice, const char* cPtxFileName)
    {
        // Grab the number of devices associated with the program
        cl_uint num_devices;
        clGetProgramInfo(cpProgram, CL_PROGRAM_NUM_DEVICES, sizeof(cl_uint), &num_devices, NULL);

        // Grab the device ids
        cl_device_id* devices = (cl_device_id*) malloc(num_devices * sizeof(cl_device_id));
        clGetProgramInfo(cpProgram, CL_PROGRAM_DEVICES, num_devices * sizeof(cl_device_id), devices, 0);

        // Grab the sizes of the binaries
        size_t* binary_sizes = (size_t*)malloc(num_devices * sizeof(size_t));
        clGetProgramInfo(cpProgram, CL_PROGRAM_BINARY_SIZES, num_devices * sizeof(size_t), binary_sizes, NULL);

        // Now get the binaries
        char** ptx_code = (char**)malloc(num_devices * sizeof(char*));
        for( unsigned int i=0; i<num_devices; ++i)
        {
            ptx_code[i] = (char*)malloc(binary_sizes[i]);
        }
        clGetProgramInfo(cpProgram, CL_PROGRAM_BINARIES, 0, ptx_code, NULL);

        // Find the index of the device of interest
        unsigned int idx = 0;
        while((idx < num_devices) && (devices[idx] != cdDevice))
        {
            ++idx;
        }

        // If the index is associated, log the result
        if(idx < num_devices)
        {

            // if a separate filename is supplied, dump ptx there
            if (NULL != cPtxFileName)
            {
                shrLog(stdlog, "\nWriting ptx to separate file: %s ...\n\n", cPtxFileName);
                FILE* pFileStream = NULL;
                #ifdef _WIN32
                    fopen_s(&pFileStream, cPtxFileName, "wb");
                #else
                    pFileStream = fopen(cPtxFileName, "wb");
                #endif

                fwrite(ptx_code[idx], binary_sizes[idx], 1, pFileStream);
                fclose(pFileStream);
            }
            else // log to logfile and console if no ptx file specified
            {
               shrLog(stdlog, "\n%s\nProgram Binary:\n%s\n%s\n", HDASHLINE, ptx_code[idx], HDASHLINE);
            }
        }

        // Cleanup
        free(devices);
        free(binary_sizes);
        for(unsigned int i = 0; i < num_devices; ++i)
        {
            free(ptx_code[i]);
        }
        free( ptx_code );
    }


    //using namespace std;

    void oclPrintDevInfo(cl_device_id device)
    {
        char device_string[1024];
        bool nv_device_attibute_query = false;

        // CL_DEVICE_NAME
        clGetDeviceInfo(device, CL_DEVICE_NAME, sizeof(device_string), &device_string, NULL);
        shrLogEx(stdlog, "  CL_DEVICE_NAME: \t\t\t%s\n", device_string);

        // CL_DEVICE_VENDOR
        clGetDeviceInfo(device, CL_DEVICE_VENDOR, sizeof(device_string), &device_string, NULL);
        shrLogEx(stdlog, "  CL_DEVICE_VENDOR: \t\t\t%s\n", device_string);

        // CL_DRIVER_VERSION
        clGetDeviceInfo(device, CL_DRIVER_VERSION, sizeof(device_string), &device_string, NULL);
        shrLogEx(stdlog, "  CL_DRIVER_VERSION: \t\t\t%s\n", device_string);

        // CL_DEVICE_VERSION
        clGetDeviceInfo(device, CL_DEVICE_VERSION, sizeof(device_string), &device_string, NULL);
        shrLogEx(stdlog, "  CL_DEVICE_VERSION: \t\t\t%s\n", device_string);

    #if !defined(__APPLE__) && !defined(__MACOSX)
        // CL_DEVICE_OPENCL_C_VERSION (if CL_DEVICE_VERSION version > 1.0)
        if(strncmp("OpenCL 1.0", device_string, 10) != 0)
        {
            // This code is unused for devices reporting OpenCL 1.0, but a def is needed anyway to allow compilation using v 1.0 headers
            // This constant isn't #defined in 1.0
            #ifndef CL_DEVICE_OPENCL_C_VERSION
                #define CL_DEVICE_OPENCL_C_VERSION 0x103D
            #endif

            clGetDeviceInfo(device, CL_DEVICE_OPENCL_C_VERSION, sizeof(device_string), &device_string, NULL);
            shrLogEx(stdlog, "  CL_DEVICE_OPENCL_C_VERSION: \t\t%s\n", device_string);
        }
    #endif

        // CL_DEVICE_TYPE
        cl_device_type type;
        clGetDeviceInfo(device, CL_DEVICE_TYPE, sizeof(type), &type, NULL);
        if( type & CL_DEVICE_TYPE_CPU )
            shrLogEx(stdlog, "  CL_DEVICE_TYPE:\t\t\t%s\n", "CL_DEVICE_TYPE_CPU");
        if( type & CL_DEVICE_TYPE_GPU )
            shrLogEx(stdlog, "  CL_DEVICE_TYPE:\t\t\t%s\n", "CL_DEVICE_TYPE_GPU");
        if( type & CL_DEVICE_TYPE_ACCELERATOR )
            shrLogEx(stdlog, "  CL_DEVICE_TYPE:\t\t\t%s\n", "CL_DEVICE_TYPE_ACCELERATOR");
        if( type & CL_DEVICE_TYPE_DEFAULT )
            shrLogEx(stdlog, "  CL_DEVICE_TYPE:\t\t\t%s\n", "CL_DEVICE_TYPE_DEFAULT");

        // CL_DEVICE_MAX_COMPUTE_UNITS
        cl_uint compute_units;
        clGetDeviceInfo(device, CL_DEVICE_MAX_COMPUTE_UNITS, sizeof(compute_units), &compute_units, NULL);
        shrLogEx(stdlog, "  CL_DEVICE_MAX_COMPUTE_UNITS:\t\t%u\n", compute_units);

        // CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS
        size_t workitem_dims;
        clGetDeviceInfo(device, CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS, sizeof(workitem_dims), &workitem_dims, NULL);
        shrLogEx(stdlog, "  CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS:\t%lu\n", workitem_dims);

        // CL_DEVICE_MAX_WORK_ITEM_SIZES
        size_t workitem_size[3];
        clGetDeviceInfo(device, CL_DEVICE_MAX_WORK_ITEM_SIZES, sizeof(workitem_size), &workitem_size, NULL);
        shrLogEx(stdlog, "  CL_DEVICE_MAX_WORK_ITEM_SIZES:\t%lu / %lu / %lu \n", workitem_size[0], workitem_size[1], workitem_size[2]);

        // CL_DEVICE_MAX_WORK_GROUP_SIZE
        size_t workgroup_size;
        clGetDeviceInfo(device, CL_DEVICE_MAX_WORK_GROUP_SIZE, sizeof(workgroup_size), &workgroup_size, NULL);
        shrLogEx(stdlog, "  CL_DEVICE_MAX_WORK_GROUP_SIZE:\t%lu\n", workgroup_size);

        // CL_DEVICE_MAX_CLOCK_FREQUENCY
        cl_uint clock_frequency;
        clGetDeviceInfo(device, CL_DEVICE_MAX_CLOCK_FREQUENCY, sizeof(clock_frequency), &clock_frequency, NULL);
        shrLogEx(stdlog, "  CL_DEVICE_MAX_CLOCK_FREQUENCY:\t%u MHz\n", clock_frequency);

        // CL_DEVICE_ADDRESS_BITS
        cl_uint addr_bits;
        clGetDeviceInfo(device, CL_DEVICE_ADDRESS_BITS, sizeof(addr_bits), &addr_bits, NULL);
        shrLogEx(stdlog, "  CL_DEVICE_ADDRESS_BITS:\t\t%u\n", addr_bits);

        // CL_DEVICE_MAX_MEM_ALLOC_SIZE
        cl_ulong max_mem_alloc_size;
        clGetDeviceInfo(device, CL_DEVICE_MAX_MEM_ALLOC_SIZE, sizeof(max_mem_alloc_size), &max_mem_alloc_size, NULL);
        shrLogEx(stdlog, "  CL_DEVICE_MAX_MEM_ALLOC_SIZE:\t\t%u MByte\n", (unsigned int)(max_mem_alloc_size / (1024 * 1024)));

        // CL_DEVICE_GLOBAL_MEM_SIZE
        cl_ulong mem_size;
        clGetDeviceInfo(device, CL_DEVICE_GLOBAL_MEM_SIZE, sizeof(mem_size), &mem_size, NULL);
        shrLogEx(stdlog, "  CL_DEVICE_GLOBAL_MEM_SIZE:\t\t%u MByte\n", (unsigned int)(mem_size / (1024 * 1024)));

        // CL_DEVICE_ERROR_CORRECTION_SUPPORT
        cl_bool error_correction_support;
        clGetDeviceInfo(device, CL_DEVICE_ERROR_CORRECTION_SUPPORT, sizeof(error_correction_support), &error_correction_support, NULL);
        shrLogEx(stdlog, "  CL_DEVICE_ERROR_CORRECTION_SUPPORT:\t%s\n", error_correction_support == CL_TRUE ? "yes" : "no");

        // CL_DEVICE_LOCAL_MEM_TYPE
        cl_device_local_mem_type local_mem_type;
        clGetDeviceInfo(device, CL_DEVICE_LOCAL_MEM_TYPE, sizeof(local_mem_type), &local_mem_type, NULL);
        shrLogEx(stdlog, "  CL_DEVICE_LOCAL_MEM_TYPE:\t\t%s\n", local_mem_type == 1 ? "local" : "global");

        // CL_DEVICE_LOCAL_MEM_SIZE
        clGetDeviceInfo(device, CL_DEVICE_LOCAL_MEM_SIZE, sizeof(mem_size), &mem_size, NULL);
        shrLogEx(stdlog, "  CL_DEVICE_LOCAL_MEM_SIZE:\t\t%u KByte\n", (unsigned int)(mem_size / 1024));

        // CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE
        clGetDeviceInfo(device, CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE, sizeof(mem_size), &mem_size, NULL);
        shrLogEx(stdlog, "  CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE:\t%u KByte\n", (unsigned int)(mem_size / 1024));

        // CL_DEVICE_QUEUE_PROPERTIES
        cl_command_queue_properties queue_properties;
        clGetDeviceInfo(device, CL_DEVICE_QUEUE_PROPERTIES, sizeof(queue_properties), &queue_properties, NULL);
        if( queue_properties & CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE )
            shrLogEx(stdlog, "  CL_DEVICE_QUEUE_PROPERTIES:\t\t%s\n", "CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE");
        if( queue_properties & CL_QUEUE_PROFILING_ENABLE )
            shrLogEx(stdlog, "  CL_DEVICE_QUEUE_PROPERTIES:\t\t%s\n", "CL_QUEUE_PROFILING_ENABLE");

        // CL_DEVICE_IMAGE_SUPPORT
        cl_bool image_support;
        clGetDeviceInfo(device, CL_DEVICE_IMAGE_SUPPORT, sizeof(image_support), &image_support, NULL);
        shrLogEx(stdlog, "  CL_DEVICE_IMAGE_SUPPORT:\t\t%u\n", image_support);

        // CL_DEVICE_MAX_READ_IMAGE_ARGS
        cl_uint max_read_image_args;
        clGetDeviceInfo(device, CL_DEVICE_MAX_READ_IMAGE_ARGS, sizeof(max_read_image_args), &max_read_image_args, NULL);
        shrLogEx(stdlog, "  CL_DEVICE_MAX_READ_IMAGE_ARGS:\t%u\n", max_read_image_args);

        // CL_DEVICE_MAX_WRITE_IMAGE_ARGS
        cl_uint max_write_image_args;
        clGetDeviceInfo(device, CL_DEVICE_MAX_WRITE_IMAGE_ARGS, sizeof(max_write_image_args), &max_write_image_args, NULL);
        shrLogEx(stdlog, "  CL_DEVICE_MAX_WRITE_IMAGE_ARGS:\t%u\n", max_write_image_args);

        // CL_DEVICE_SINGLE_FP_CONFIG
        cl_device_fp_config fp_config;
        clGetDeviceInfo(device, CL_DEVICE_SINGLE_FP_CONFIG, sizeof(cl_device_fp_config), &fp_config, NULL);
        shrLogEx(stdlog, "  CL_DEVICE_SINGLE_FP_CONFIG:\t\t%s%s%s%s%s%s\n",
            fp_config & CL_FP_DENORM ? "denorms " : "",
            fp_config & CL_FP_INF_NAN ? "INF-quietNaNs " : "",
            fp_config & CL_FP_ROUND_TO_NEAREST ? "round-to-nearest " : "",
            fp_config & CL_FP_ROUND_TO_ZERO ? "round-to-zero " : "",
            fp_config & CL_FP_ROUND_TO_INF ? "round-to-inf " : "",
            fp_config & CL_FP_FMA ? "fma " : "");

        // CL_DEVICE_IMAGE2D_MAX_WIDTH, CL_DEVICE_IMAGE2D_MAX_HEIGHT, CL_DEVICE_IMAGE3D_MAX_WIDTH, CL_DEVICE_IMAGE3D_MAX_HEIGHT, CL_DEVICE_IMAGE3D_MAX_DEPTH
        size_t szMaxDims[5];
        shrLogEx(stdlog, "\n  CL_DEVICE_IMAGE <dim>");
        clGetDeviceInfo(device, CL_DEVICE_IMAGE2D_MAX_WIDTH, sizeof(size_t), &szMaxDims[0], NULL);
        shrLogEx(stdlog, "\t\t\t2D_MAX_WIDTH\t %lu\n", szMaxDims[0]);
        clGetDeviceInfo(device, CL_DEVICE_IMAGE2D_MAX_HEIGHT, sizeof(size_t), &szMaxDims[1], NULL);
        shrLogEx(stdlog, "\t\t\t\t\t2D_MAX_HEIGHT\t %lu\n", szMaxDims[1]);
        clGetDeviceInfo(device, CL_DEVICE_IMAGE3D_MAX_WIDTH, sizeof(size_t), &szMaxDims[2], NULL);
        shrLogEx(stdlog, "\t\t\t\t\t3D_MAX_WIDTH\t %lu\n", szMaxDims[2]);
        clGetDeviceInfo(device, CL_DEVICE_IMAGE3D_MAX_HEIGHT, sizeof(size_t), &szMaxDims[3], NULL);
        shrLogEx(stdlog, "\t\t\t\t\t3D_MAX_HEIGHT\t %lu\n", szMaxDims[3]);
        clGetDeviceInfo(device, CL_DEVICE_IMAGE3D_MAX_DEPTH, sizeof(size_t), &szMaxDims[4], NULL);
        shrLogEx(stdlog, "\t\t\t\t\t3D_MAX_DEPTH\t %lu\n", szMaxDims[4]);

        // CL_DEVICE_EXTENSIONS: get device extensions, and if any then parse & log the string onto separate lines
        clGetDeviceInfo(device, CL_DEVICE_EXTENSIONS, sizeof(device_string), &device_string, NULL);
        if (device_string != 0)
        {
            shrLogEx(stdlog, "\n  CL_DEVICE_EXTENSIONS:");
            std::string stdDevString;
            //string stdDevString;
            stdDevString = std::string(device_string);
            size_t szOldPos = 0;
            size_t szSpacePos = stdDevString.find(' ', szOldPos); // extensions string is space delimited
            while (szSpacePos != stdDevString.npos)
            {
                if( strcmp("cl_nv_device_attribute_query", stdDevString.substr(szOldPos, szSpacePos - szOldPos).c_str()) == 0 )
                    nv_device_attibute_query = true;

                if (szOldPos > 0)
                {
                    shrLogEx(stdlog, "\t\t");
                }
                shrLogEx(stdlog, "\t\t\t%s\n", stdDevString.substr(szOldPos, szSpacePos - szOldPos).c_str());

                do {
                    szOldPos = szSpacePos + 1;
                    szSpacePos = stdDevString.find(' ', szOldPos);
                } while (szSpacePos == szOldPos);
            }
            shrLogEx(stdlog, "\n");
        }
        else
        {
            shrLogEx(stdlog, "  CL_DEVICE_EXTENSIONS: None\n");
        }

        if(nv_device_attibute_query)
        {
#ifndef __APPLE__
            cl_uint compute_capability_major, compute_capability_minor;
            clGetDeviceInfo(device, CL_DEVICE_COMPUTE_CAPABILITY_MAJOR_NV, sizeof(cl_uint), &compute_capability_major, NULL);
            clGetDeviceInfo(device, CL_DEVICE_COMPUTE_CAPABILITY_MINOR_NV, sizeof(cl_uint), &compute_capability_minor, NULL);
            shrLogEx(stdlog, "\n  CL_DEVICE_COMPUTE_CAPABILITY_NV:\t%u.%u\n", compute_capability_major, compute_capability_minor);
#endif
            shrLogEx(stdlog, "  NUMBER OF MULTIPROCESSORS:\t\t%u\n", compute_units); // this is the same value reported by CL_DEVICE_MAX_COMPUTE_UNITS
#ifndef __APPLE__
            shrLogEx(stdlog, "  NUMBER OF CUDA CORES:\t\t\t%u\n", ConvertSMVer2CoresCopy(compute_capability_major, compute_capability_minor) * compute_units);

            cl_uint regs_per_block;
            clGetDeviceInfo(device, CL_DEVICE_REGISTERS_PER_BLOCK_NV, sizeof(cl_uint), &regs_per_block, NULL);
            shrLogEx(stdlog, "  CL_DEVICE_REGISTERS_PER_BLOCK_NV:\t%u\n", regs_per_block);

            cl_uint warp_size;
            clGetDeviceInfo(device, CL_DEVICE_WARP_SIZE_NV, sizeof(cl_uint), &warp_size, NULL);
            shrLogEx(stdlog, "  CL_DEVICE_WARP_SIZE_NV:\t\t%u\n", warp_size);

            cl_bool gpu_overlap;
            clGetDeviceInfo(device, CL_DEVICE_GPU_OVERLAP_NV, sizeof(cl_bool), &gpu_overlap, NULL);
            shrLogEx(stdlog, "  CL_DEVICE_GPU_OVERLAP_NV:\t\t%s\n", gpu_overlap == CL_TRUE ? "CL_TRUE" : "CL_FALSE");

            cl_bool exec_timeout;
            clGetDeviceInfo(device, CL_DEVICE_KERNEL_EXEC_TIMEOUT_NV, sizeof(cl_bool), &exec_timeout, NULL);
            shrLogEx(stdlog, "  CL_DEVICE_KERNEL_EXEC_TIMEOUT_NV:\t%s\n", exec_timeout == CL_TRUE ? "CL_TRUE" : "CL_FALSE");

            cl_bool integrated_memory;
            clGetDeviceInfo(device, CL_DEVICE_INTEGRATED_MEMORY_NV, sizeof(cl_bool), &integrated_memory, NULL);
            shrLogEx(stdlog, "  CL_DEVICE_INTEGRATED_MEMORY_NV:\t%s\n", integrated_memory == CL_TRUE ? "CL_TRUE" : "CL_FALSE");
#endif
        }

        // CL_DEVICE_PREFERRED_VECTOR_WIDTH_<type>
        shrLogEx(stdlog, "  CL_DEVICE_PREFERRED_VECTOR_WIDTH_\t");
        cl_uint vec_width [6];
        clGetDeviceInfo(device, CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR, sizeof(cl_uint), &vec_width[0], NULL);
        clGetDeviceInfo(device, CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT, sizeof(cl_uint), &vec_width[1], NULL);
        clGetDeviceInfo(device, CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT, sizeof(cl_uint), &vec_width[2], NULL);
        clGetDeviceInfo(device, CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG, sizeof(cl_uint), &vec_width[3], NULL);
        clGetDeviceInfo(device, CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT, sizeof(cl_uint), &vec_width[4], NULL);
        clGetDeviceInfo(device, CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE, sizeof(cl_uint), &vec_width[5], NULL);
        shrLogEx(stdlog, "CHAR %u, SHORT %u, INT %u, LONG %u, FLOAT %u, DOUBLE %u\n\n\n",
               vec_width[0], vec_width[1], vec_width[2], vec_width[3], vec_width[4], vec_width[5]);
    }

    void oclLogBuildInfo(cl_program cpProgram, cl_device_id cdDevice)
    {
        // write out the build log and ptx, then exit
        char cBuildLog[10240];
        clGetProgramBuildInfo(cpProgram, cdDevice, CL_PROGRAM_BUILD_LOG,
                              sizeof(cBuildLog), cBuildLog, NULL );
        shrLog(stdlog, "\n%sBuild Log:\n%s\n%s\n", HDASHLINE, cBuildLog, HDASHLINE);
    }

    cl_device_id oclGetDevFromContext(cl_context cxGPUContext, cl_int dev_id)
    {
        size_t szParmDataBytes;
        cl_device_id* cdDevices;

        // get the list of GPU devices associated with context
        clGetContextInfo(cxGPUContext, CL_CONTEXT_DEVICES, 0, NULL, &szParmDataBytes);
        cdDevices = (cl_device_id*) malloc(szParmDataBytes);

        clGetContextInfo(cxGPUContext, CL_CONTEXT_DEVICES, szParmDataBytes, cdDevices, NULL);

        cl_device_id dev = cdDevices[dev_id];
        free(cdDevices);

        return dev;
    }



/****************************************************************/
/*********************** BUILD OCL PROGRAM **********************/
/****************************************************************/

using namespace std;

void build_for_GPU(
        cl_context&         cxGPUContext,
        cl_command_queue*   cqCommandQueue,
        cl_uint&            nDevice,
        cl_device_id*&      cdDevices,
        cl_program&         cpProgram,
        unsigned int        dev_id,
        const char*         compileOptions,
        const char*         preamble,
        string              name
    ) {
        cl_platform_id  cpPlatform;  // OpenCL platform
        cl_int          ciErr1;
        string           cl_name = name;
        string          ptx_name = name;

        // build the cl file and ptx file names
        cl_name  += ".cl";
        ptx_name += ".ptx";

        // 2. get the platforms' ids
        shrLog(stdlog, "Get platforms...\n");
        ciErr1 = oclGetPlatformID(&cpPlatform);
        oclCheckError(ciErr1, CL_SUCCESS);

        // 3. get devices
        shrLog(stdlog, "Get devices...\n");
        ciErr1 = clGetDeviceIDs(cpPlatform, CL_DEVICE_TYPE_GPU, 0, NULL, &nDevice);
        oclCheckError(ciErr1, CL_SUCCESS);
        cdDevices = (cl_device_id *)malloc(nDevice * sizeof(cl_device_id) );
        ciErr1 = clGetDeviceIDs(cpPlatform, CL_DEVICE_TYPE_GPU, nDevice, cdDevices, NULL);
        oclCheckError(ciErr1, CL_SUCCESS);
	shrLog(stdlog, "Got %d devices:\n", nDevice);
	#define MAX_STRING_LENGTH 1024
	char buf[MAX_STRING_LENGTH];
	for(cl_uint i = 0; i < nDevice;i++) {
	  clGetDeviceInfo(cdDevices[i], CL_DEVICE_NAME, MAX_STRING_LENGTH, buf, NULL);
          shrLog(stdlog, "  Device %d: %s, which supports ", i, buf);
          clGetDeviceInfo(cdDevices[i], CL_DEVICE_VERSION, MAX_STRING_LENGTH, buf, NULL);
          shrLog(stdlog, "%s\n", buf); 
        }
        if (nDevice < 1) {
          shrLog(stdlog, "Exiting: No GPU devices available\n");
          exit(1);
        }
        shrLog(stdlog, "Using device %d (change by modifying your platform.mk file)\n", dev_id); 

        // 4. create context
        shrLog(stdlog, "Create context with all devices...\n");
        cxGPUContext = clCreateContext(NULL, nDevice, cdDevices, NULL, NULL, &ciErr1);
        oclCheckError(ciErr1, CL_SUCCESS);

        // 5. create command queues for all available devices
        for (cl_uint i = 0; i < nDevice; i++) {
	    shrLog(stdlog, "Creating command queue for device %d\n", i);
            cqCommandQueue[i] = clCreateCommandQueue(cxGPUContext, cdDevices[i], 0, &ciErr1);
            oclCheckErrorEX(ciErr1, CL_SUCCESS, NULL);
        }

#ifdef DEBUG_PRINT_GPU_INFO
        for (cl_uint i = 0; i < nDevice; i++) {
            oclPrintDevInfo(cdDevices[i]);
        }
#endif
        // 6. build program
        shrLog(stdlog, "Create and build program from %s...\n", cl_name.c_str());

        size_t szKernelLength; // Byte size of kernel code
        assert(cl_name.c_str() != NULL && "UNDEFINED cSourcePath");
        char * kernel_code = oclLoadProgSource(cl_name.c_str(), preamble, &szKernelLength);

        oclCheckError(kernel_code != NULL, shrTRUE);
        shrLog(stdlog, "Program source code loaded...\n");

        cpProgram = clCreateProgramWithSource(cxGPUContext, 1, (const char **)&kernel_code, &szKernelLength, &ciErr1);
	if (ciErr1 != CL_SUCCESS)
	  {
	    shrLog(stdlog, "Error in clCreateProgramWithSource, Line %u in file %s !!!\n\n", __LINE__, __FILE__);
	    exit(0);
	  }

        shrLog(stdlog, "Program created...\n\n");

        //shrLog(stdlog, "Before building error: %d, code: %d, success: %d, num_dev: %d\n\n",
        //      ciErr1, CL_BUILD_PROGRAM_FAILURE, CL_SUCCESS, nDevice);

        ciErr1 = clBuildProgram(cpProgram, 1, cdDevices+dev_id, compileOptions, NULL, NULL);

        // 7. check errors!
        if (ciErr1 != CL_SUCCESS) {
            // write out standard error, Build Log and PTX, then cleanup and exit
            shrLog(stdlog, "BUILDING ERROR: %d: %s\n", ciErr1, oclErrorString(ciErr1));
            //oclLogBuildInfo(cpProgram, cdDevices[dev_id]);

            //oclLogPtx(cpProgram, cdDevices[dev_id], ptx_name.c_str());

            if (ciErr1 == CL_BUILD_PROGRAM_FAILURE) {
                // Determine the size of the log
                size_t log_size;
                clGetProgramBuildInfo(cpProgram, cdDevices[dev_id], CL_PROGRAM_BUILD_LOG, 0, NULL, &log_size);
                // Allocate memory for the log
                char *log = (char *) malloc(log_size);
                // Get the log
                clGetProgramBuildInfo(cpProgram, cdDevices[dev_id], CL_PROGRAM_BUILD_LOG, log_size, log, NULL);
                // Print the log
                shrLog(stdlog, "%s\n", log);
            }

            oclCheckError(ciErr1, CL_SUCCESS);
        }
    #if 0
        {
            size_t workgroup_size;
            clGetDeviceInfo(cdDevices[dev_id], CL_DEVICE_MAX_WORK_GROUP_SIZE, sizeof(workgroup_size), &workgroup_size, NULL);
            shrLogEx(stdlog, "  COSMIN!!!! CL_DEVICE_MAX_WORK_GROUP_SIZE:\t%lu\n", workgroup_size);
        }
    #endif

        shrLog(stdlog, "Program built...\n");

    }

#endif // end SDK_INTERNALS

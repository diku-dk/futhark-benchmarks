#include "context_setup.h"

void lys_setup_futhark_context(const char *deviceopt, bool device_interactive,
                               struct futhark_context_config* *futcfg,
                               struct futhark_context* *futctx,
                               char* *opencl_device_name) {
  *futcfg = futhark_context_config_new();
  assert(*futcfg != NULL);

#if defined(LYS_BACKEND_opencl) || defined(LYS_BACKEND_cuda)
  if (deviceopt != NULL) {
    futhark_context_config_set_device(*futcfg, deviceopt);
  }
#else
  (void)deviceopt;
#endif

#ifdef LYS_BACKEND_opencl
  if (device_interactive) {
    futhark_context_config_select_device_interactively(*futcfg);
  }
#else
  (void)device_interactive;
#endif

  *futctx = futhark_context_new(*futcfg);
  assert(*futctx != NULL);

#ifdef LYS_BACKEND_opencl
  cl_device_id device;
  assert(clGetCommandQueueInfo(futhark_context_get_command_queue(*futctx),
                               CL_QUEUE_DEVICE, sizeof(cl_device_id), &device, NULL)
         == CL_SUCCESS);

  size_t dev_name_size;
  assert(clGetDeviceInfo(device, CL_DEVICE_NAME, 0, NULL, &dev_name_size)
         == CL_SUCCESS);
  *opencl_device_name = malloc(dev_name_size);
  assert(clGetDeviceInfo(device, CL_DEVICE_NAME, dev_name_size, *opencl_device_name, NULL)
         == CL_SUCCESS);
#else
  *opencl_device_name = NULL;
#endif
}

int64_t lys_wall_time() {
  struct timeval time;
  assert(gettimeofday(&time,NULL) == 0);
  return time.tv_sec * 1000000 + time.tv_usec;
}

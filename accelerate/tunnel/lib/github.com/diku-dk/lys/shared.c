#include "shared.h"
#include <string.h>

const char* basename(const char *progname) {
  int n = strlen(progname);
  int i;
  for (i = n-1; i >= 0; i--) {
    if (progname[i] == '/') {
      return &progname[i+1];
    }
  }
  return &progname[i+1];
}

void lys_setup_futhark_context(const char *progname,
                               const char *deviceopt, bool device_interactive,
                               struct futhark_context_config* *futcfg,
                               struct futhark_context* *futctx,
                               char* *opencl_device_name) {
  *futcfg = futhark_context_config_new();
  assert(*futcfg != NULL);

#if defined(FUTHARK_BACKEND_opencl) || defined(FUTHARK_BACKEND_cuda)
  if (deviceopt != NULL) {
    futhark_context_config_set_device(*futcfg, deviceopt);
  }
#else
  (void)deviceopt;
#endif

#ifdef FUTHARK_BACKEND_opencl
  if (device_interactive) {
    futhark_context_config_select_device_interactively(*futcfg);
  }
#else
  (void)device_interactive;
#endif

  if (progname != NULL) {
    int bufsize = strlen(progname) + 10;
    char buf[bufsize];
    // Would it be better to store this in some XDG directory?
    snprintf(buf, bufsize, "%s.lyscache", basename(progname));
    futhark_context_config_set_cache_file(*futcfg, buf);
  }

  *futctx = futhark_context_new(*futcfg);
  assert(*futctx != NULL);

#ifdef FUTHARK_BACKEND_opencl
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

#ifdef LYS_TEXT
size_t n_printf_arguments();

void prepare_text(struct futhark_context* futctx, struct lys_text *text) {
  struct futhark_u8_1d *text_format_array;
  FUT_CHECK(futctx, futhark_entry_text_format(futctx, &text_format_array));
  size_t text_format_len = futhark_shape_u8_1d(futctx, text_format_array)[0];
  text->text_format = malloc(sizeof(char) * (text_format_len + 1));
  assert(text->text_format != NULL);
  FUT_CHECK(futctx, futhark_values_u8_1d(futctx, text_format_array, (unsigned char*) text->text_format));
  FUT_CHECK(futctx, futhark_context_sync(futctx));
  text->text_format[text_format_len] = '\0';
  FUT_CHECK(futctx, futhark_free_u8_1d(futctx, text_format_array));

  text->sum_names = (char* **) malloc(sizeof(char* *) * n_printf_arguments());
  assert(text->sum_names != NULL);

  text->text_buffer_len = text_format_len;
  size_t i_arg = -1;
  for (size_t i = 0; i < text_format_len; i++) {
    if (text->text_format[i] == '%' &&
        i + 1 < text_format_len && text->text_format[i + 1] != '%') {
      i_arg++;
      if (text->text_format[i + 1] == '[') {
        text->text_format[i + 1] = 's';
        size_t end_pos;
        size_t n_choices = 1;
        bool found_end = false;
        for (end_pos = i + 2; end_pos < text_format_len; end_pos++) {
          if (text->text_format[end_pos] == '|') {
            n_choices++;
          } else if (text->text_format[end_pos] == ']') {
            found_end = true;
            break;
          }
        }
        assert(found_end);
        text->sum_names[i_arg] = (char* *) malloc(sizeof(char*) * (n_choices + 1));
        assert(text->sum_names[i_arg] != NULL);
        text->sum_names[i_arg][n_choices] = NULL;
        char* temp_choice = (char*) malloc(sizeof(char) * (end_pos - i - n_choices));
        assert(temp_choice != NULL);
        size_t choice_cur = 0;
        size_t i_choice = 0;
        for (size_t j = i + 2; j < end_pos + 1; j++) {
          if (text->text_format[j] == '|' || text->text_format[j] == ']') {
            temp_choice[choice_cur] = '\0';
            text->sum_names[i_arg][i_choice] = (char*) malloc(sizeof(char) * (choice_cur + 1));
            assert(text->sum_names[i_arg][i_choice] != NULL);
            strncpy(text->sum_names[i_arg][i_choice], temp_choice, choice_cur + 1);
            choice_cur = 0;
            i_choice++;
          } else {
            temp_choice[choice_cur] = text->text_format[j];
            choice_cur++;
          }
        }
        free(temp_choice);
        size_t shift_left = end_pos - i - 1;
        for (size_t j = end_pos + 1; j < text_format_len; j++) {
          text->text_format[j - shift_left] = text->text_format[j];
        }
        text_format_len -= shift_left;
        text->text_format[text_format_len] = '\0';
        i++;
      } else {
        text->sum_names[i_arg] = NULL;
        text->text_buffer_len += 20; // estimate
      }
    }
  }

  text->text_buffer = malloc(sizeof(char) * text->text_buffer_len);
  assert(text->text_buffer != NULL);
  text->text_buffer[0] = '\0';

  text->show_text = true;
}
#endif

// dl-open.c
#include <dlfcn.h>

void* dl_open (const char* path, int mode)
{
  int new_mode = 0;

  if (mode & 1) new_mode |= RTLD_LAZY;
  if (mode & 2) new_mode |= RTLD_NOW;
  if (mode & 4) new_mode |= RTLD_LOCAL;
  if (mode & 8) new_mode |= RTLD_GLOBAL;

  return dlopen (path, new_mode);
}

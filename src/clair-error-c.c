/*
 * @file clair-error-c.c
 * @brief C standard library wrapper functions for Ada bindings.
 *
 * Copyright (c) 2025 Hodong Kim <hodong@nimfsoft.art>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
/**
 * @file
 * @brief Enables POSIX.1-2008 standard features.
 *
 * This macro must be defined before including any system headers.
 *
 * It instructs the C library (like glibc) to expose function prototypes
 * and data structures that conform to the POSIX.1-2008 standard. This is
 * crucial for ensuring portability and predictable behavior across different
 * systems.
 *
 * A key example is `strerror_r`, which has two different versions:
 * 1. A POSIX-compliant version that returns an `int`.
 * 2. A GNU-specific version that returns a `char*`.
 *
 * By defining `_POSIX_C_SOURCE` with the value `200809L`, we ensure that the
 * compiler sees the POSIX version, which provides a consistent and
- * standardized interface. This is essential for writing portable code or
 * creating reliable bindings for other languages like Ada.
 */
#define _POSIX_C_SOURCE 200809L

#include <errno.h>
#include <string.h>
#include <stdio.h>

/**
 * @brief Returns the current value of the `errno` variable.
 *
 * @return The current `errno` value.
 *
 * @note This wrapper function is necessary for ensuring portability and correctness
 * when accessing `errno` from other languages, such as Ada.
 *
 * The C standard defines `errno` as a macro that expands to a modifiable lvalue.
 * In many modern C libraries, especially in multi-threaded environments, this macro
 * resolves to a function call (e.g., `*__errno_location()`) to provide a
 * thread-local error number.
 *
 * Directly importing "errno" as a variable from another language is not portable
 * and may fail or lead to incorrect behavior if `errno` is implemented as such a
 * macro. This function abstracts away the platform-specific implementation
 * details, providing a stable and reliable interface to retrieve the current
 * thread's `errno` value.
 */
int clair_error_get_errno (void)
{
  return errno;
}

/**
 * @brief Provides a portable and consistent interface to the thread-safe
 * strerror_r function.
 *
 * @param errnum The error number to look up.
 * @param buf The buffer to store the error message string.
 * @param buflen The size of the buffer.
 *
 * @return 0 on success, or a non-zero error code (e.g., EINVAL, ERANGE) on
 * failure. The resulting string is always stored in `buf`.
 *
 * @note This wrapper is crucial for portability because there are two
 * different and incompatible versions of `strerror_r`:
 *
 * 1.  **The POSIX (XSI-compliant) version**:
 * `int strerror_r(int errnum, char *buf, size_t buflen);`
 * This version returns an integer (0 for success) and always uses the
 * caller-provided buffer.
 *
 * 2.  **The GNU-specific version**:
 * `char *strerror_r(int errnum, char *buf, size_t buflen);`
 * This version returns a `char*` pointing to the error string. This string
 * may or may not be the one in the caller's buffer.
 *
 * This wrapper abstracts away this difference, providing a single, consistent
 * interface that behaves like the POSIX version. This ensures that code
. * calling it (e.g., from Ada) will work correctly on any platform,
 * regardless of the underlying `strerror_r` implementation.
 */
 #if defined(__GLIBC__)
  #include <gnu/libc-version.h>
    #if __GLIBC__ < 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ < 13)
    #error "Error: glibc version 2.13 or later is required."
  #endif
#endif
int clair_error_strerror_r (int errnum, char* buf, size_t buflen)
{
  return strerror_r (errnum, buf, buflen);
}

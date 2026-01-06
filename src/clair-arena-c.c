/*
 * clair-arena-c.c
 * Wrapper functions for jemalloc macros to be used in Ada.
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

#ifdef __FreeBSD__
  #include <malloc_np.h>
  #include <strings.h>
#else
  #include <jemalloc/jemalloc.h>
#endif

/*
 * Wrapper for the MALLOCX_ARENA(a) macro
 * Input: arena index
 * Output: jemalloc flag
 */
unsigned int clair_get_arena_flag (unsigned int arena_index)
{
  return MALLOCX_ARENA (arena_index);
}

/*
 * Wrapper for the MALLOCX_ALIGN(a) macro
 * Input: alignment in bytes (e.g., 64)
 * Output: jemalloc flag (flag converted to a log value)
 */
unsigned int clair_get_align_flag (size_t alignment)
{
  return MALLOCX_ALIGN (alignment);
}